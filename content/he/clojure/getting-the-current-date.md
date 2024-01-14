---
title:                "Clojure: קבלת תאריך נוכחי"
simple_title:         "קבלת תאריך נוכחי"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# למה:

רובנו רוצים לדעת מה התאריך הנוכחי בכל פעם שאנו משתמשים בתכנית כתיבה, יצירת אפליקציות או כל פעילות אחרת שדורשת ייצוג של התאריך הנוכחי. כתיבת תאריך ושעה באופן ידני כל פעם מבסס על כך ועשה אותו בצורה חוזרת יכול להיות מסוכן וטוב לתכניות גדולות יותר.

# כיצד לעשות זאת:

השתמש בפונקציה ```(java.util.Date.)``` כדי לקבל את התאריך הנוכחי בתוך תא בקוד שלכם.

```Clojure
(defn current-date []
  "פונקציה זו מחזירה את התאריך הנוכחי."
  (let [today (java.util.Date.)]
    (.toString today)))

(current-date)
;;output: "Sat Sep 25 01:41:51 IDT 2021"
```

במקום להניח שהמשתנה שלנו ```today``` יוצא מסוג Date מהעבר, אנו משתמשים בפונקציה כדי ליצור אותו במהירות ולקבל את התאריך הנוכחי.

# מעמקים:

אנו יכולים גם להשתמש בפונקציות נוספות כדי להתאים את התאריך לפי החשיבה לפני כתיבתו. לדוגמה, נוסיף את התאריך הנוכחי על כל שאלות התאריך שלנו כדי לקבל תשובות מדויקות יותר.

```Clojure
(defn current-time []
  "פונקציה זו מחזירה את השעה הנוכחית."
  (let [today (java.util.Date.)
        fmt (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")]
    (.format fmt today)))

(current-time)
;;output: "2021-09-25 01:44:16"
```

ניתן גם להשתמש במגוון פונקציות המספקות את התאריך והשעה בפורמט שלנו, לדוגמה ```(format "%A, %B %d, %Y" (java.util.Date.))```.

# ראה גם:

- [מדריך לתאריך וזמן ב-Clojure](http://www.lispcast.com/clojure-date-time)
- [התעלל כולל של תיאוריית הפונקציה מנקודת מבט של Clojure](http://www.braveclojure.com/writing-macros/)
- [תיע