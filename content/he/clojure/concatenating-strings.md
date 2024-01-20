---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

# מה זה ולמה?
מקשר כלות או "concatenation" הוא למעשה הדבקה של מחרוזות יחד. נעשה זאת כדי ליצור מידע חדש או כדי לעבד מחרוזות בחישובים.

# איך לעשות את זה:
להלן כמה דוגמאות של מינוח למקצוענים בשפת Clojure:

```Clojure
(def str1 "שלום, ")
(def str2 "עולם!")
(str str1 str2)
```

הפלט של הקוד הזה יהיה:

```Clojure
"שלום, עולם!"
```

הפונקציה str מקשרת את המחרוזות str1 ו-str2 כדי ליצור מחרוזת חדשה.

# צלילה עמוקה
אך מהן בדיוק הפרטים שאנחנו צריכים לדעת על צירוף מחרוזות ב-Clojure?

1. בהקשר היסטורי: התכנות הפונקציונלי, בו הוא מתבצע Clojure, מתמקד בהסתפקות בערכים לעיבוד נתונים, דבר שמחשיב את צירוף המחרוזות לדי נפוצ בשפות תכנות אחרות.

2. חלופות: אתה יכול להשתמש בפונקציית הצירוף של Clojure, אך אלהרנטיבה אחרת היא הפונקציה `clojure.string/join`.

```Clojure
(def list-of-strings ["שלום, " "עולם!"])
(clojure.string/join "" list-of-strings)
```

הפונקציה `clojure.string/join` מבצעת יעילות גבוהה יותר בתוך רשימות מחרוזות ארוכות יותר.

3. מידע על המימוש: הפונקציה `str` מתרגמת את המחרוזות שלה לפונקציית צירוף של ה-Java JVM הקיימת, מה שמביא לביצועים מהירים ואמינים.

# ראה גם
1. [הדרכת תחילה של Clojure](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
2. [תיעוד Clojure](https://clojuredocs.org/)