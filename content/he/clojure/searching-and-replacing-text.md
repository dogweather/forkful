---
title:                "Clojure: חיפוש והחלפת טקסטים"
simple_title:         "חיפוש והחלפת טקסטים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה

למה אנשים מתעסקים בחיפוש והחלפה של טקסט? זה די פשוט - מספר רב של מטרות ומטרות קונקרטיות. זה עשוי להיות לשפר קוד קיים, לשנות את מבנה הטקסט של קבצים או לבצע הפרדות ושיורים בין מילים או תווים אחרים.

## איך לעשות זאת

תיעוד של Clojure מציע כמה כלים שיכולים לעזור בנתונים סטרימס. הנה כמה דוגמאות של קוד כדי להציג את כיצד להשתמש בכלים אלה:

```Clojure
; החלפת מילה בכל הטקסט
(str/replace "שלום עולם!" "עולם" "מגינס")

; החלפת תווים מסוימים בטקסט
(str/replace "0,1,2,3,4,5" #"[\d]" "number")

; החלפה של אותיות קטנות באותיות גדולות
(str/replace "Hello" #"[a-z]" (fn [m] (.toUpperCase (str m))))
```

הפלט הנכון יהיה:

```Clojure
"שלום מגינס"
"number,number,number,number,number,number"
"HELLO"
```

## לעשות צילום מכות

כאשר בא לעצב אלגנטי, עיבוד כיליות ותיעוד המתמקדים בהחלפת קטעים טקסט, יש לקבוע את התהליך בפרספקטיבה העמוקה יותר. ישנם כמה מילוניוםטרות המציעות תועלת נרחבת כאשר מדובר על החלפת טקסט:

- [תיעוד של מדריך התחביר של קלוז'ר](https://clojure.org/guides/string_syntax_guide)
- [ספריה מתומכת ייעודית לניהול טקסט](https://clojure.github.io/string)
- [מקומון המכיל נוסחאות גרמל מינימליות](https://github.com/Engelberg/instaparse)
- [מאמר מתקדם על נושא מהאתר הרשמי של קלוז'ר](https://clojuredocs.org/clojure.string/replace)

## ראה גם

- [תיעוד של מדריך התחביר של קלוז'ר](https://clojure.org/guides/string_syntax_guide)
- [ס