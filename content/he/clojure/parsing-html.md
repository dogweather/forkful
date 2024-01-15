---
title:                "ניתוח שפת תגיות HTML"
html_title:           "Clojure: ניתוח שפת תגיות HTML"
simple_title:         "ניתוח שפת תגיות HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## למה

למה להתעסק בפרסום HTML? הפרסום של HTML הוא כלי מאוד חשוב לפיתוח תוכנה, לכן זה יכול להיות כוח עוצמתי בידי המתכנתים. באמצעות פרסום HTML, ניתן ליצור תכניות ואתרים דינמיים עם תצורות מתחשבות, תשתיות פשוטות יותר ופיצול אחריות.

## איך לעשות זאת

הוראות קוד ותצורות פרסום לפרסום HTML כפי שאתה לא חייב להיות יכול לכתוב עם שפת התכנות Clojure. הנה דוגמה:

קבלת דף HTML והדפסת תוכן הדף:
```clojure
(use 'clojure.string)
(def page (slurp "https://www.example.com"))
(println page)
```

מילוי תוכן טופס HTML עם פרפרים:
```clojure
(require '[clojure.xml :as xml])
(require '[clojure.zip :as zip])

(def forms
  (xml-> (zip/xml-zip (xml/parse "https://www.example.com"))
    :form))
   
(println (xml-zip/xml-> 
  (xml-zip/xml-zipper forms)
  :form
  (xml-zip/attr :action)
  :content))
```

הנה דוגמא נוספת עם פרסום תוכן כתמלול משומע:
```clojure
(require '[enlive.core :as html])

(html/defsnippet form "form" [form action]
  [[:input {:type "hidden" 
            :name "form-id" 
            :value  (str (int (rand)))}]]
  [:input {:class "form-title" 
           :type "text" 
           :name "form-title" 
           :autocomplete "off"}])

(html/emit* (form "https://www.example.com/submit") {:title "My Form"})
```

## צליל עמוק

הפרסום של HTML בעזרת Clojure משמש כקלט וכפלט, כך שניתן לבנות מערכות בכלים שונים כדי להכין את היישום המתאים לך. בעזרת כלים נוספים כמו המפוחם המעניק מידע על דפי אינטרנט קשורים נוספים, להחיל האלגורטמים לסינונים אקראי או לכתוב ולבדוק את מה שכתובת האתר שלך מרכיבה מקבועה. קוד של Clojure שנועד עבור זה נוכל לעשות כוון של תוצאם ראשית לכתוב את קצת דפ