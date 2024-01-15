---
title:                "עבודה עם JSON"
html_title:           "Clojure: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## למה
תהליך של עבודה עם JSON נחשב לכלי חיוני לפיתוח תוכניות בשפת Clojure ומאפשר מעבר נעים וקל בין מבני מידע שונים.

## כיצד
הנה כמה דוגמאות לעבודה עם JSON בשפת Clojure ותוצאות הפלט המתאימות בתוך בלוקי קוד "```Clojure ... ```":

```Clojure
;; יצירת מפתח-ערך
(def json-map {"name" "John" "age" 25})

;; הדפסת תוצאה בפורמט JSON
(print (json/write-str json-map))

;; {"name":"John","age":25}
```

```Clojure
;; יצירת רשימה של מפתחות-ערכים
(def json-list [{"name" "John"} {"name" "Jane"}])

;; קריאת קובץ JSON והמרתו לרשימה
(def json-file (slurp "data.json"))
(def json-list (json/read-str json-file))

;; שינוי ערכים במפתח-ערך מסוים והדפסת התוצאה
(let [new-json-map (assoc-in (first json-list) "name" "Jack")]
  (print (json/write-str new-json-map)))

;; {"name":"Jack"}
```

## העומק
עבודה עם JSON בשפת Clojure יכולה להיות מסובכת כאשר מגיעים לקבצים מורכבים עם מפתחות נוספים או כאשר ניתן לקרוא רק מספר מוגבל של ערכים בתוך רשימה או מפתח-ערך בודד. במקרים כאלה, כדאי להשתמש בספריות חיצוניות כגון jsonista או clj-json.

## ראה גם
- [תיעוד על עבודה עם JSON בשפת Clojure](https://clojure.github.io/data.json/)
- [עורך טקסט חינמי לשפת Clojure עם תמיכה מובנית ב-JSON](https://atom.io/packages/clojure-json)