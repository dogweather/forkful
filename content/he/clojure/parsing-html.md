---
title:                "Clojure: המרת HTML לנתונים מחשב"
simple_title:         "המרת HTML לנתונים מחשב"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/parsing-html.md"
---

{{< edit_this_page >}}

מדוע: רק 1-2 משפטים לסביר את הסיבה לכך שמישהו ירצה לעסוק בניתוח HTML.

מינוארי: הדגמת קוד ופלט בתוך חסימות "``Clojure ...```". 

```
(def html "<html>
<body>
<h1>Hello, World!</h1>
<p>This is a sample HTML document</p>
</body>
</html>") ;; דוגמה לקוד HTML

(require '[clojure.string :as str])
(require '[clojure.data.xml :as xml]) 

(-> html 
    str/replace #"<\/?.+?>" "")
    ;; ניקוי התגים מתוך הטקסט המקורי

(-> html 
    xml/parse 
    :content 
    xml/root)

;; {:tag :html,
;;  :attrs nil,
;;  :content
;;   [{:tag :body,
;;     :attrs nil,
;;     :content
;;     [{:tag :h1,
;;       :attrs nil,
;;       :content
;;       [{:tag :text, :attrs nil, :content "Hello, World!"}]}
;;      {:tag :p,
;;       :attrs nil,
;;       :content
;;       [{:tag :text,
;;         :attrs nil,
;;         :content "This is a sample HTML document"}]}]}]}
```

עומק מים: מידע מעמיק על ניתוח HTML.

ניתוח HTML הוא תהליך המאפשר לנו להפוך טקסט בפורמט HTML למבנה נתונים הנוח יותר לעיבוד ועיבוד נוסף. את התהליך הזה ניתן לבצע בעזרת מספר ביבליות של Clojure, כגון `clojure.data.xml` או `enlive`, המאפשרות לנו לקרוא ולכתוב טקסט HTML בצורה נוחה ופשוטה. בנוסף, ניתוח HTML מאפשר לנו לעבד ולקשר נתונים ממקורות שונים, כגון אתרי אינטרנט, נתוני API ועוד.

ראה גם:

- תיעוד רשמי של `clojure.data.xml`: https://github.com/clojure/data.xml
- תיעוד רשמי של `enlive`: https://github.com/cgrand/enlive
- מדריכים ותכניות לימוד על ניתוח HTML ב-Clojure: https://github.com/learn-clojure/clojure-web-dev-resources/blob/master/5-parsing-html.md