---
title:                "קבלת התאריך הנוכחי"
html_title:           "Clojure: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

נתארח בהמשך חקירה שמטרתה מציאת דרך לקבלת התאריך הנוכחי בשפת Clojure. אם אתם מתעניינים בכתיבת תוכניות בשפת Clojure ומרגישים שזה יימרץ או יותר מעורפל ממה שצריך, אנחנו ממליצים לכם להתחיל בפעילות הרגילה שלכם בהפסקה קצרה ופשוטה כשאתם מחפשים דרך לקבלת התאריך הנוכחי.

## איך לעשות זאת
בשפת Clojure, ישנן מספר דרכים שונות לקבלת התאריך הנוכחי. בכדי להציג הבנת טיפוסים שונים וטכניקות תכנותיות, נדגים את השימוש בפונקציות "new" ו"now". לפניכם שתי דרכים שונות לקבלת התאריך הנוכחי בשפת Clojure.

```Clojure
;;ראשית, נשתמש בפונקציה new
(defn get-current-date []
  (let [current-date (java.util.Date.)] ;קובעים משתנה חדש עם התאריך הנוכחי
    (.toString current-date))) ;שימוש בפונקציה toString כדי להמיר את התאריך למחרוזת
  ```


שנית, נעשה שימוש בפונקציה now שמגיעה עם המודול java-time.

```Clojure
;;שימוש ב־import כדי ליבא את המודול java-time
(import 'java.time.LocalDateTime)

(defn get-current-date []
  (let [current-date (LocalDateTime/now)] ;תאריך נוכחי כפי שמחליטה הפונקציה now של java-time
    (.toString current-date))) ;שימוש בפונקציה toString כדי להמיר את התאריך למחרוזת
```

בשתי הדרכים, אנחנו משתמשים בפונקציה toString כדי להמיר את התאריך למחרוזת כדי להציג אותו בכתב טקסט. אתם יכולים לשנות את הפורמט של התאריך בעזרת פונקציות נוספות של java-time כמו "format" או להשתמש בספריית שליטה של המודול.

## צלילה עמוקה
שפת Clojure מספ