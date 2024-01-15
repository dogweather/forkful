---
title:                "חיפוש והחלפת טקסטים"
html_title:           "Clojure: חיפוש והחלפת טקסטים"
simple_title:         "חיפוש והחלפת טקסטים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## על מה עושים את זה

ממשת ב- Clojure כולל חיפוש והחלפת טקסט היא דרך נוחה ויעילה לשנות את התוכן של קבצים. זה יכול לשמור על מוצא זמן ומאמץ במידה ואת/ה נאלצים לשנות טקסט בכמה מקומות באותו זמן.

## איך לעשות את זה

הנה כמה דוגמאות לשימוש בפונקציות של Clojure לחיפוש והחלפת טקסט:

```Clojure
;; חיפוש והחלפת מחרוזת ספציפית במחרוזת אחרת
(str/replace "שלום, מה שלומך?" "שלום" "היי")
;; התוצאה תהיה: "היי, מה שלומך?"

;; חיפוש והחלפת מחרוזת בשימוש בפונקציה מותאמת אישית
(defn replace-vowels [text]
  (str/replace text #"([aeiou])" "-"))
(replace-vowels "לאן אתה הולך?")
;; התוצאה תהיה: "ל-?נ -תה ה-?לך?"

;; חיפוש והחלפת מחרוזת בפייפרינט
(println (str/replace "אני אוהב גלישה" "אוהב" "מתחבר"))
;; התוצאה תהיה: "אני מתחבר גלישה"
```

## הכנסה עמוקה

פונקציות החיפוש וההחלפה של Clojure ניתנות להתאמה אישית בכמה רמות. ניתן להשתמש בפונקציות מובנות כמו `str/replace` או `str/replace-first` או לבנות פונקציות מותאמות אישית עם ביטויים רגולריים. רשימת הפונקציות הזמינות נמצאת בתיעוד הרשמי של Clojure.

## ראו גם

- המאמר הפופולרי של Clojure על מימוש חיפוש והחלפה: https://clojure.org/guides/text_processing
- פונקציות חיפוש והחלפה במדריך הרשמי של Clojure: https://clojure.org/reference/strings#searchreplace
- לימודי Clojure נוספים שאפשר למצוא באינטרנט: https://lincolnloop.com/blog/clojure-web-development/