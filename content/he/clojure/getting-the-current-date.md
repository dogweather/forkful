---
title:                "Clojure: קבלת התאריך הנוכחי"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# למה: רק 1-2 משפטים המסבירים *למה* מישהו יחזור על קבלת התאריך הנוכחי

התכנית הזו מציגה איך לקבל ולשים בתוך משתנה, את התאריך הנוכחי עם עזרת שפת התכנות Clojure.

## איך לעשות זאת:

לאחר התקנת ליבררי Clojure, ניתן להתחיל ליצור קוד דימוי תאריך הנוכחי בכמה צעדים פשוטים:

```Clojure
;; משתנה שמחזיק את התאריך הנוכחי
(def current-date (java.util.Date.))

;; תאריך בפורמט של יום שבוע, חודש ושנה
(def formatted-date (java.text.SimpleDateFormat. "dd-MM-yyyy").format current-date)

;; תאריך בפורמט של חציון
(def formatted-date (java.text.SimpleDateFormat. "MMyy").format current-date)
```

הפלט של התוכנית הזו יהיה משתני התאריך הנוכחיים בפורמטים שונים, שניתן להתאים לפי הצורך של המשתמש.

## מכתב עמוק:

כדי להבין טוב יותר כיצד התוכנית מפעילה את תאריך היום הנוכחי, ניתן לבצע חקר עמוק יותר על הפונקציות שנמצאות בשפת התכנות Clojure. ניתן לראות כי פקודת (java.util.Date) מייצרת את תאריך היום הנוכחי מתוך האובייקט java.util.Date ואזמנתית של האובייקט לפונקציה של SimpleDateFormat. בנוסף, ניתן להתאים את הפורמט הנמצא לפני הפונקציה לפי הצורך שלנו באופן שונה.

# ראו גם:

- [מדריך להתחיל עם Clojure](https://www.clojure.org/guides/getting_started)
- [תיעוד רשמי לשפת Clojure](https://clojure.org/api/api)
- [פונקציות ופורמטים בשפת Clojure](https://clojuredocs.org/core-library)