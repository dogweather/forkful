---
title:                "הגדלת אותיות במחרוזת"
aliases: - /he/clojure/capitalizing-a-string.md
date:                  2024-02-03T19:05:21.119114-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
