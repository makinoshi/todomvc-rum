(ns todomvc-rum.core
  (:require [rum.core :as rum]))

(enable-console-print!)

;; Constants

(def enter-key-code 13)

(def app-elm (.getElementById js/document "app"))

;; DB

(def initial-db
  {:input-text ""
   :todo-list []})

(def db (atom initial-db))

(def input-text (rum/cursor-in db [:input-text]))

(def todo-list (rum/cursor-in db [:todo-list]))

;; Compoent

(rum/defc app < rum/reactive
  []
  [:section.todoapp
   [:div
    [:header.header
     [:h1 "todos"]
     [:input.new-todo {:auto-focus true
                       :placeholder "What needs to be done?"
                       :value (rum/react input-text)
                       :on-change (fn [e] (reset! input-text (.. e -target -value)))
                       :on-key-down (fn [e] (when (and (= (.-keyCode e) enter-key-code)
                                                      (seq @input-text))
                                             (swap! todo-list conj @input-text)
                                             (reset! input-text "")))}]]
    [:section.main
     [:input#toggle-all.toggle-all {:type :checkbox}]
     [:label {:for "toggle-all"}]
     [:ul.todo-list
      (map-indexed (fn [i s]
                     [:li {:key i}
                      [:input.toggle {:type :checkbox}]
                      [:label s]
                      [:button.destroy]])
                   @todo-list)]
     #_[:input.edit {:type :text}]]
    (when (seq @todo-list)
      [:footer.footer
       [:span.todo-count
        [:strong (count @todo-list)]
        [:span " item left"]]
       [:ul.filters
        [:li [:a.selected {:href "#/"} "ALL"]]
        [:li [:a {:href "#/active"} "Active"]]
        [:li [:a {:href "#/completed"} "Completed"]]]])]])

(reset! db initial-db)
(rum/mount (app) app-elm)
