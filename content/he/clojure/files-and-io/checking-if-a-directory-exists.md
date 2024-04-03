---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:03.448127-07:00
description: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-Clojure \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05D0\u05D9\u05DE\u05D5\u05EA \u05E0\u05D5\u05DB\u05D7\u05D5\u05EA \u05E9\
  \u05DC \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05DE\u05E2\u05E8\u05DB\u05EA \u05E7\
  \u05D1\u05E6\u05D9\u05DD \u05DE\u05EA\u05D5\u05DA \u05D9\u05D9\u05E9\u05D5\u05DD\
  \ Clojure \u05E9\u05DC\u05DA. \u05DE\u05E9\u05D9\u05DE\u05D4 \u05D6\u05D5 \u05E7\
  \u05E8\u05D9\u05D8\u05D9\u05EA \u05DC\u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E7\
  \u05D5\u05D1\u05E5, \u05DB\u05D3\u05D9 \u05DC\u05DE\u05E0\u05D5\u05E2 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA \u05D1\u05E2\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4\u2026"
lastmod: '2024-03-13T22:44:38.728300-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-Clojure \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05E0\u05D5\u05DB\u05D7\u05D5\u05EA \u05E9\u05DC\
  \ \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05DE\u05E2\u05E8\u05DB\u05EA \u05E7\u05D1\
  \u05E6\u05D9\u05DD \u05DE\u05EA\u05D5\u05DA \u05D9\u05D9\u05E9\u05D5\u05DD Clojure\
  \ \u05E9\u05DC\u05DA."
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
weight: 20
---

## מה ולמה?
בדיקה אם ספרייה קיימת ב-Clojure כוללת אימות נוכחות של ספריית מערכת קבצים מתוך יישום Clojure שלך. משימה זו קריטית לפעולות קובץ, כדי למנוע שגיאות בעת קריאה מתוך ספריות או כתיבה אליהן שעשויות שלא להיות קיימות, מה שמבטיח ביצוע קוד רציף וחף משגיאות.

## איך לעשות:
Clojure, בתור שפה של JVM, יכולה להשתמש במחלקה `java.io.File` של Java למטרה זו. אין צורך בספרייה צד שלישי לפעולה בסיסית כזו. הנה איך אפשר לעשות זאת:

```clojure
(import 'java.io.File)

(defn directory-exists? [dir-path]
  (let [dir (File. dir-path)]
    (.exists dir)))

;; דוגמה לשימוש
(println (directory-exists? "/path/to/your/directory")) ;; true או false
```

פונקציה זו, `directory-exists?`, לוקחת נתיב ספרייה כמחרוזת ומחזירה `true` אם הספרייה קיימת ו-`false` אחרת. זה מושג על ידי יצירת אובייקט `File` עם נתיב הספרייה ואז קריאה למתודה `.exists` על אובייקט זה.

בנוסף לעבודה ישירה עם Java, ניתן להשתמש בספריות Clojure שמפשטות חלק מהקוד הגנרי של Java. ספרייה אחת כזו היא `clojure.java.io`. עם זאת, לצורך בדיקה אם ספרייה קיימת, עדיין תשתמשו במחלקה `File`, אך יתכן שתמצאו את הספרייה שימושית לפעולות קובץ אחרות. דוגמה:

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists?-clojure [dir-path]
  (.exists (io/file dir-path)))

;; דוגמה לשימוש
(println (directory-exists?-clojure "/another/path/to/check")) ;; true או false
```

גרסה זו דומה מאוד אך משתמשת בפונקציה `io/file` של Clojure ליצירת אובייקט ה`File`. שיטה זו משתלבת בצורה טבעית יותר בבסיסי קוד של Clojure על ידי ניצול ספריית Clojure לפעולות IO, במקום לעבוד ישירות עם מחלקות Java.
