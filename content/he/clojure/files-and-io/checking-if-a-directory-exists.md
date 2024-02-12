---
title:                "בדיקה אם ספרייה קיימת"
aliases:
- /he/clojure/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:03.448127-07:00
model:                 gpt-4-0125-preview
simple_title:         "בדיקה אם ספרייה קיימת"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
