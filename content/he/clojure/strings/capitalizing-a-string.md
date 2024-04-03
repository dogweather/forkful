---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:21.119114-07:00
description: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA \u05E8\u05D0\u05E9\
  \u05D5\u05E0\u05D4 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA (Capitalizing) \u05DB\
  \u05D5\u05DC\u05DC\u05EA \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05DB\u05DA \u05E9\u05D4\u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\
  \u05D5\u05DF \u05E9\u05DC\u05D4 \u05D9\u05D4\u05D9\u05D4 \u05D1\u05D0\u05D5\u05EA\
  \ \u05D2\u05D3\u05D5\u05DC\u05D4, \u05D1\u05E2\u05D5\u05D3 \u05E9\u05E9\u05D0\u05E8\
  \ \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E0\u05E9\u05D0\u05E8\u05EA \u05DC\
  \u05DC\u05D0 \u05E9\u05D9\u05E0\u05D5\u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA\
  \u2026"
lastmod: '2024-03-13T22:44:38.676948-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA \u05E8\u05D0\u05E9\u05D5\
  \u05E0\u05D4 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA (Capitalizing) \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05DB\u05DA \u05E9\u05D4\u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\
  \u05DF \u05E9\u05DC\u05D4 \u05D9\u05D4\u05D9\u05D4 \u05D1\u05D0\u05D5\u05EA \u05D2\
  \u05D3\u05D5\u05DC\u05D4, \u05D1\u05E2\u05D5\u05D3 \u05E9\u05E9\u05D0\u05E8 \u05D4\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E0\u05E9\u05D0\u05E8\u05EA \u05DC\u05DC\
  \u05D0 \u05E9\u05D9\u05E0\u05D5\u05D9."
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 2
---

## מה ולמה?
הגדלת אות ראשונה במחרוזת (Capitalizing) כוללת שינוי המחרוזת כך שהתו הראשון שלה יהיה באות גדולה, בעוד ששאר המחרוזת נשארת ללא שינוי. מתכנתים לעיתים קרובות מבצעים הגדלת אות ראשונה במחרוזת כדי להבטיח אחידות נתונים, במיוחד לשמות ומקומות או לצורך התאמה לכללים דקדוקיים בממשקי משתמש.

## איך לעשות זאת:
Clojure, בהיותו שפת JVM, מאפשר לך להשתמש ישירות בשיטות מחרוזת של Java. הנה דוגמה בסיסית לכיצד לבצע הגדלת אות ראשונה במחרוזת ב-Clojure:

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojure לא כולל פונקציה מובנית ספציפית להגדלת אות ראשונה במחרוזת, אך כפי שנראה, ניתן להשיג זאת בקלות על ידי שילוב שיטות `clojure.string/upper-case`, `subs`, ו-`str`.

לפתרון קצר יותר ולטיפול במניפולציות מחרוזת מורכבות יותר, ייתכן שתפנה לספרייה של צד שלישי. אחת הספריות הפופולריות באקוסיסטם של Clojure היא `clojure.string`. עם זאת, לאחרונה, היא לא מציעה פונקציית `capitalize` ישירה מעבר למה שהוצג עם פונקציונליות הליבה של Clojure, כך שהשיטה המוצגת לעיל היא הגישה הישירה שלך ללא צורך בכללת ספריות נוספות במיוחד להגדלת אות.

זכור, כאשר אתה עובד עם מחרוזות ב-Clojure המתקשרות עם שיטות Java, אתה למעשה עובד עם מחרוזות Java, מה שמאפשר לך לנצל את כל ארסנל שיטות המחרוזת של Java ישירות בקוד ה-Clojure שלך, במידת הצורך.
