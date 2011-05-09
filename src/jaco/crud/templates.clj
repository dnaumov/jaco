(ns jaco.crud.templates
  (:use (hiccup core form-helpers [page-helpers :only [link-to]])))

;; TODO: templates -- 09.05.11

(defn default-view "Default view function for fields - textbox"
  [name value] (html (text-field name value)))


(defn completed []
  (html
   [:h1 "done!"]))

(defn error [messages]
  (html
   [:h1 "error!"]
   [:ul
    (for [m messages]
      [:li m])]))


(defn view
  [id fields] {:pre [(apply = [:title :comment :view] (map keys fields))]}
  (html
   [:h1 "View Item"]
   (form-to [:post ""]
     (hidden-field :id id)
     (for [{:keys [title comment view]} fields]
       [:div.row
        [:div.desc
         [:div.title title]
         [:div.comment comment]]
        [:div.view view]])
     (submit-button "Save"))))

(defn overview "entries - coll of [update-url delete-url entry-map]"
  [entries create-url]
  (html
   [:h1 "Entries list"]
   [:table
    (for [[update-url delete-url entry] entries]
      [:tr
       (for [[key val] entry]
         [:td val])
       [:td (link-to update-url "Edit")]
       [:td (link-to delete-url "Delete")]])]
   (link-to create-url "Create new one")))

(defn index [coll]
  (html
   [:h1 "CRUD Module"]
   [:table
    (for [{:keys [title url]} coll]
      [:tr
       [:td (link-to url title)]])]))
