---
title:                "כתיבת קובץ טקסט"
html_title:           "Clojure: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?:
כתיבת קובץ טקסט היא פעולתה של יום-יום של מתכנתים. מתכנתים משתמשים בכתיבת קבצי טקסט כדי ליצור קוד מבואר ונקרא, וכן לייצר תיעוד ומסמכים אחרים. הגדרה זו אולי נשמעת ברורה עבור הפועל "כתיבה", אבל ניתן לסבך את הדרישה להבין בסיסיים נוספים של כתיבת קבצי טקסט על ידי ליצירת טקסט שאינו פשוט כל כך.

בכתיבת קבצי טקסט אנו נמצאים על ה"כתב" של מדינה דיגיטלית. אנו יכולים ליצור, לכתוב ולעדכן קבצי טקסט בכל זמן ומכל ציוד כמו מחשבים, סמארטפונים וטאבלטים.

## How to:
תחת כותרת זו, אנחנו מציגים שלושה דוגמאות לקוד קורטקטי ששימושו בכתיבת טקסט נכון ופשוט.

כתיבת טקסט פשוט מאוד:


כדי לכתוב טקסט פשוט ב-Clojure, השתמש בפונקציית `spit` המצפה עבורך קובץ טקסט חדש אליו תוכל לכתוב תוכן. לדוגמה:

```Clojure
(spit "new-file.txt" "This is a simple text file.")
```

כדי לקרוא טקסט מקובץ מסוים, אנו יכולים להשתמש בפונקציית `slurp` כדי לקרוא את הטקסט ולהציג אותו למשתמש. לדוגמה:

```Clojure
(println (slurp "input-file.txt"))
```

כתיבת מסמך אתר דינמי:

ביצוע כתיבת טקסט משמעותי ומקושר יכול להיות אתגר, אבל זה קל מאוד לכתוב מצגת דינמית שמשתמשת בכתיבת טקסט בתוכנה שם. במטרה להציג כללי תהליך, כל קבץ אחר נתמך. לדוגמה, בעזרת הקוד הבא, אנו יכולים ליצור מצגת דינמית שמשתמשת בטבלאות והשוואות ב-HTML.

```Clojure
(defn get-table-header [arr]
  (clojure.string/join "\t" arr))

(defn demarcate [arr]
  (clojure.string/join "|" arr))

(def comparison-table
  [:headings ["Clojure" "Python" "Java"]
   :body
   [[1 2 3] [4 5 6] [7 8 9]]
   [[: "Cats" "Dogs" "Birds"]
    ["Fish" "Turtles" "Ducks"]]])

(clojure.string/join "\n"
  (map (fn [comparison]
         (demarcate (map get-table-header comparison)))
       comparison-table))
```

Output:

```
"Clojure|Python|Java"
"1 Cats|2 Dogs|3 Birds"
"4 Fish|5 Turtles|6 Ducks"
"7 Cats|8 Dogs|9 Birds"
"10 Fish|11 Turtles|12 Ducks"
```

## Deep Dive:
כתיבת טקסט נעשית על ידי טכנולוגיות רבות, אולם פשטנו את הנושא ופרטנו על כתיבת טקסט ב-Clojure בלבד. אין ספק שסגירת קובץ טקסט באמצעות גרסאות ייחודיות של Clojure יותר נעשה על ידי פונקציות כ