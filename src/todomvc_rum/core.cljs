(ns todomvc-rum.core
  (:require [clojure.string :as str]
            [rum.core :as rum]))

(enable-console-print!)

;; Constants

(def enter-key-code 13)

(def app-elm (.getElementById js/document "app"))

;; DB

(def initial-db
  {:input-text ""
   :todo-list []
   :disp-status :all
   :editing-index nil})

(def db (atom initial-db))

(def input-text (rum/cursor-in db [:input-text]))

(def todo-list (rum/cursor-in db [:todo-list]))

(def disp-status (rum/cursor-in db [:disp-status]))

(def editing-index (rum/cursor-in db [:editing-index]))

;; DB operation

(defn- add-new-task [text]
  (swap! todo-list conj {:text text :status :active}))

(defn- remove-task [index]
  (->> @todo-list
       (keep-indexed (fn [i m]
                       (when-not (= i index)
                         m)))
       vec
       (reset! todo-list)))

(defn- change-task-status [index checked?]
  (swap! todo-list assoc-in [index :status] (if checked? :completed :active)))

(defn- toggle-all [checked?]
  (let [status (if checked? :completed :active)]
    (->> @todo-list
         (mapv #(assoc % :status status))
         (reset! todo-list))))

(defn- clear-completed []
  (->> @todo-list
       (remove #(= (:status %) :completed))
       vec
       (reset! todo-list)))

(defn- start-edit [index]
  (reset! editing-index index))

(defn- end-edit []
  (reset! editing-index nil))

(defn- update-task-text [index text]
  (swap! todo-list assoc-in [index :text] text))

;; Compoent

(rum/defc input-task < rum/reactive
  []
  [:input.new-todo {:auto-focus true
                    :placeholder "What needs to be done?"
                    :value (rum/react input-text)
                    :on-change (fn [e] (reset! input-text (.. e -target -value)))
                    :on-key-down (fn [e] (when (and (= (.-keyCode e) enter-key-code)
                                                   (seq @input-text))
                                          (add-new-task @input-text)
                                          (reset! input-text "")))}])

(rum/defc todo-item <
  rum/reactive
  {:key-fn (fn [i _ _] (str "task-" i))}
  [i text status]
  [:li
   {:class (str/join " " (remove nil? [(when (= status :completed) "completed")
                                       (when (= (rum/react editing-index) i) "editing")]))}
   [:div.view
    [:input.toggle {:type :checkbox
                    :checked (= status :completed)
                    :on-change (fn [e] (change-task-status i (.. e -target -checked)))}]
    [:label {:on-double-click (fn [_]
                                (js/console.log "on-double-click")
                                (start-edit i))}
     text]
    [:button.destroy {:on-click (fn [_] (remove-task i))}]]
   [:input.edit {:type :text
                 :value text
                 :focus (str (= (rum/react editing-index) i))
                 :on-change (fn [e] (update-task-text i (.. e -target -value)))
                 :on-key-down (fn [e] (when (= (.-keyCode e) enter-key-code)
                                       (end-edit)
                                       (when (empty? text)
                                         (remove-task i))))
                 :on-blur (fn [_]
                            (js/console.log "on-blur")
                            (end-edit))}]])

(rum/defc footer < rum/reactive
  []
  (when (seq (rum/react todo-list))
    [:footer.footer
     [:span.todo-count
      [:strong (->> (rum/react todo-list)
                    (filter #(= (:status %) :active))
                    count)]
      [:span " item left"]]
     [:ul.filters
      [:li {:on-click (fn [_] (reset! disp-status :all))}
       [(if (= (rum/react disp-status) :all) :a.selected :a)
        {:href "#/"} "ALL"]]
      [:li {:on-click (fn [_] (reset! disp-status :active))}
       [(if (= (rum/react disp-status) :active) :a.selected :a)
        {:href "#/active"} "Active"]]
      [:li {:on-click (fn [_] (reset! disp-status :completed))}
       [(if (= (rum/react disp-status) :completed) :a.selected :a)
        {:href "#/completed"} "Completed"]]]
     (when (seq (filter #(= (:status %) :completed) (rum/react todo-list)))
       [:button.clear-completed
        {:on-click (fn [_] (clear-completed))}
        "Clear completed"])]))

(rum/defc app < rum/reactive
  []
  [:section.todoapp
   [:div
    [:header.header
     [:h1 "todos"]
     (input-task)]
    [:section.main
     [:input#toggle-all.toggle-all {:type :checkbox
                                    :on-change (fn [e] (toggle-all (.. e -target -checked)))}]
     [:label {:for "toggle-all"}]
     [:ul.todo-list
      (loop [i 0
             [{:as m :keys [text status]} :as coll] (rum/react todo-list)
             ret []]
        (if-not m
          ret
          (if (or (= (rum/react disp-status) :all)
                  (= (rum/react disp-status) status))
            (recur (inc i)
                   (rest coll)
                   (conj ret (todo-item i text status)))
            (recur (inc i)
                   (rest coll)
                   ret))))]]
    (footer)]])

(reset! db initial-db)
(rum/mount (app) app-elm)
