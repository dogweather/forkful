---
date: 2024-01-26 03:49:19.150137-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : Clojure \u05E0\u05E9\u05E2\u05E0\u05EA \u05E2\u05DC \u05DE\u05DB\u05D5\u05E0\u05EA\
  \ \u05D4\u05D6\u05DE\u05DF \u05D4\u05D5\u05D5\u05D9\u05E8\u05D8\u05D5\u05D0\u05DC\
  \u05D9\u05EA \u05E9\u05DC Java (JVM), \u05DC\u05DB\u05DF \u05D4\u05E8\u05D1\u05D4\
  \ \u05DE\u05D4\u05E0\u05D9\u05E4\u05D5\u05D9 \u05E9\u05DC \u05D4\u05D1\u05D0\u05D2\
  \u05D9\u05DD \u05E0\u05E2\u05E9\u05D4 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\
  \ \u05DB\u05DC\u05D9\u05DD \u05E9\u05DC Java. \u05DB\u05DC\u05D9 \u05D0\u05D7\u05D3\
  \ \u05DB\u05D6\u05D4 \u05D4\u05D5\u05D0 `CIDER`, \u05D7\u05D1\u05D9\u05DC\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.711655-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u05E0\u05E9\u05E2\u05E0\u05EA \u05E2\u05DC \u05DE\u05DB\u05D5\u05E0\
  \u05EA \u05D4\u05D6\u05DE\u05DF \u05D4\u05D5\u05D5\u05D9\u05E8\u05D8\u05D5\u05D0\
  \u05DC\u05D9\u05EA \u05E9\u05DC Java (JVM), \u05DC\u05DB\u05DF \u05D4\u05E8\u05D1\
  \u05D4 \u05DE\u05D4\u05E0\u05D9\u05E4\u05D5\u05D9 \u05E9\u05DC \u05D4\u05D1\u05D0\
  \u05D2\u05D9\u05DD \u05E0\u05E2\u05E9\u05D4 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\
  \u05EA \u05DB\u05DC\u05D9\u05DD \u05E9\u05DC Java."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA"
weight: 35
---

## איך לעשות זאת:
Clojure נשענת על מכונת הזמן הווירטואלית של Java (JVM), לכן הרבה מהניפוי של הבאגים נעשה באמצעות כלים של Java. כלי אחד כזה הוא `CIDER`, חבילת על לפיתוח Clojure ב-Emacs, שיש לה יכולות ניפוי תקלות מוצקות. בואו נצלול פנימה:

```clojure
;; ראשית, תתחברו לפרויקט Clojure בתוך Emacs באמצעות CIDER
M-x cider-jack-in

;; הגדרת נקודת עצירה
;; נווטו לשורה בקוד ה-Clojure שלכם שאתם רוצים לבדוק ו
;; לחצו על "C-c M-b" או בצעו:
M-x cider-debug-defun-at-point

;; כאשר הקוד רץ, תפגעו בנקודת העצירה. CIDER תציג בפניכם את האפשרויות:
;; 1. n כדי לעבור לשלב הלוגי הבא בביצוע,
;; 2. c כדי להמשיך בביצוע עד לנקודת עצירה הבאה,
;; 3. q כדי להפסיק את ניפוי התקלות.

;; בדיקת משתנים מקומיים בנקודת עצירה
;; בעת היותך בנקודת עצירה, הקלד:
locals

;; תראו רשימה של משתנים מקומיים והערכים שלהם מודפסים ב-minibuffer.
```
דוגמא לפלט עשויה להיראות כך:
```clojure
{:x 10, :y 20, :result 200}
```

## צלילה עמוקה
המנפה הוא כלי כמו קדמוני המחשבים במונחים של חישוב. המונח "באג" התגלה לראשונה בימים הראשונים של המחשבים, כאשר חרק בפועל גרם לשגיאה על ידי קצר במעגל במכונה.

למרות ש-`CIDER` מעולה לחובבי Emacs, קיימות חלופות לניפוי באגים ב-Clojure. לדוגמה, השימוש ב-IntelliJ עם התוסף Cursive יכול להציע חוויית ניפוי תקלות שמבוססת יותר על GUI. נוסף לזאת, אתם יכולים להשתמש ב-Leiningen המובנה או tools.deps כדי לשלוט על זרם התהליך בזמן הניפוי.

מאחורי הקלעים, מנפים אלו לעיתים קרובות מתעסקים בשינוי קודי בייט, ביצוע חישובים בהפעלות nREPL מוקדשות, ומציעים בדיקת עקבות קריאה. הם מנצלים את היכולות של JVM הקיימת להם מתחת למכסה, ונכנסים לעומק רובדי הניפוי של Java.

## ראו גם
- [תיעוד המנפה של CIDER](https://docs.cider.mx/cider/debugging/debugger.html)
- [מנפה של Cursive](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen לאוטומציה וניפוי](https://leiningen.org/)
- [tools.deps.alpha ליותר שליטה](https://github.com/clojure/tools.deps.alpha)
