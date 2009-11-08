(ns issue-migrator
  (:import (java.net URLEncoder))
  (:use clojure.contrib.http.agent)
  (:use clojure.xml)
  (:use clojure.contrib.prxml)
  (:use clojure.contrib.duck-streams))

(def *appname* "Clojure Google Code Issue Migrator")

;Fill in these details if you want to be able to submit tickets
(def *userdetails* {"Email" "youremail" "Passwd" "yourpassword" })

;URL to retrieve issues from 
(def *source* "http://code.google.com/feeds/issues/p/clojure-dev/issues/full")
;URL to post issues to
(def *destination* "http://code.google.com/feeds/issues/p/counterclockwise/issues/full")

(defn url-encode [param]
  (URLEncoder/encode param))

(defn construct-param-string
  "Construct a URL parameter string out of the given parameter map. Separates
  keys and values with \"=\" and key-value pairs with an ampersand. All
  values are also URL encoded." 
  [params]
  (apply str (interpose "&" (map str (keys params) (repeat "=") (map url-encode (vals params)))))) 

(defn google-auth-request [userdetails]
  (let [client-login-url "https://www.google.com/accounts/ClientLogin"
        appparams {"accountType" "GOOGLE" "source" *appname* "service" "code"}
        params (merge userdetails appparams)
        body (construct-param-string params)]
        (string (http-agent client-login-url :method "POST" :body body))))

(defn get-auth-token [userdetails]
  (let [response (google-auth-request userdetails)]
    (nth (first (re-seq #"Auth=([\w-]*)\n" response)) 1)))


;Get the xml from the old clojure-dev issues list
(def xml (parse "http://code.google.com/feeds/issues/p/clojure-dev/issues/full"))

(defn get-tags [xml tag]
  (filter #(= (:tag %) tag) xml))
  
(def tickets (get-tags (:content xml) :entry))

(defn get-tag-content 
  "Gets the contents of the first element in the given
  xml with the given tag name"
  [xml tag]
  (:content (first (get-tags xml tag))))

(defn print-ticket [ticket]
  (println (str "Title: " (get-tag-content ticket :title)))
  (println (str "Author: " (get-tag-content (get-tag-content ticket :author) :name)))
  (println (str "Owner: " (get-tag-content (get-tag-content ticket :issues:owner) :issues:username)))
  (println (str "Labels: " (seq (map :content (get-tags ticket :issues:label)))))
  (println (str "State: " (get-tag-content ticket :issues:state)))
  (println (str "Status: " (get-tag-content ticket :issues:status)))
  (println (str "Description: " (get-tag-content ticket :content))))


;The required xml attributes to associate with each ticket
(def entry-attrs {:xmlns "http://www.w3.org/2005/Atom", :xmlns:issues "http://schemas.google.com/projecthosting/issues/2009"})

;We need to make sure that tickets assigned to Laurent
;are correctly recognised by the API so convert his email to his username
(defn fix-laurents-email [text]
  (.replaceAll text "laurent....@gmail.com" "laurent.petit"))


(defn set-entry-attrs [entry]
  (assoc entry :attrs entry-attrs))

(def new-tickets (map set-entry-attrs tickets))

(defn prompt [msg]
  (do 
    (println msg)
    (first (remove empty? (read-lines *in*)))))

(defn printresponse [agt]
  (await agt)
  (println (str "Response code: " (status agt)))
  (println (str "Response message: " (message agt)))
  (println (str "Response: " (string agt))))

(defn submit-ticket [ticket auth-token]
  (print-ticket (:content ticket))
  (when (= "yes" (prompt (str "Submit ticket to " *destination* " ?")))
    (let [ticketxml (fix-laurents-email (with-out-str (emit (set-entry-attrs ticket))))]
      (do 
        (println (str "Submitting ticket to " *destination* "..."))
        (printresponse 
          (http-agent *destination* 
            :method "POST" 
            :headers [["Content-type" "application/atom+xml"]["Authorization" (str "GoogleLogin auth=" auth-token)]] 
            :body ticketxml))
        (println)))))

(defn submit-tickets [tickets auth-token]
  (doseq [ticket tickets]
    (submit-ticket ticket auth-token)))

;(submit-tickets tickets (get-auth-token *userdetails*))
