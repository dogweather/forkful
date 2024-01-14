---
title:                "Clojure: המרת תאריך למחרוזת"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

על למה: להסביר ב-1-2 משפטים *למה* מישהו ירצה להמיר תאריך למחרוזת.

## לאיך לכתוב קוד:
 להמחר את הפלט של הקוד תוך שמירה על הפורמט מתאים בתוך בלוקי קוד "רצף..." קוד אקווריאלי.

```Clojure
(defn convert-date [date]
  (str (get date :month) "/" (get date :day) "/" (get date :year)))

(def sample-date {:month 04 :day 21 :year 2021})

(convert-date sample-date)

```

תוצאה: "04/21/2021"

## העברה לרמה הבאה:
 למעמד יותר עמוק על הפונקציונליות של המרה של תאריך למחרוזת. מידע נוסף על תכונות נוספות של מרה תאריכים למחרוזת יכול לסייע לפתרון בעיות עתידיות.

### דוגמאות נוספות:
1) שימוש בפונקציות נוספות כגון (format-time) עם השתמשות בתבניות שונות למיקוד בתאריך.

שם פונקציה: 

```Clojure
(defn convert-date-custom [date format]
  (format-time format date))
```

שימוש בתבנית:

```Clojure
(format-time "MM/dd/YY" sample-date)
```

תוצאה: "04/21/21"

2) שימוש בפונקציות להמרת ערכים מספריים למחרוזת כדי לשלב ערכים נוספים כמו יום בשבוע או שעה.

שם פונקציה:

```Clojure
(defn convert-date-extra [date]
  (str (get date :month) "/" (get date :day) "/" (get date :year) " at " (format-time "hh:mm a" sample-date)))
```

תוצאה: "04/21/2021 at 09:00 PM"

## לראות גם:
למידע נוסף על פונקציות שקשורות למרה של תאריך למחרוזת, יש לראות את הקישורים הבאים:

- [הדרכה לבניית אפליקציות תוך שימוש בשפת Clojure](https://www.ipix.co.il/todgeetfa/2)
- [Java ו-Clojure – אפשרויות תפוסה של שימוש משותף](https://www.geektime.co.il/clojure-truth-2)