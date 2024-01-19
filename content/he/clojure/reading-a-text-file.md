---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה & למה?
קריאת קובץ טקסט היא התהליך שבו מסופר תוכן או נתונים מתוך קובץ טקסט. מתכנתים עושים זאת לאיסוף, לניתוח או לשימוש במידע המאוחסן בקובץ.

## איך:
הקוד הבא מתחיל בהראת כיצד להשתמש בפונקציה 'slurp' בשפת Clojure כדי לקרוא קובץ טקסט:
```Clojure
(def text (slurp "myfile.txt"))
(prn text)
```
כאשר 'myfile.txt' הוא השם של הקובץ שאתה רוצה לקרוא. הפונקציה ‚slurp‛ תקרא את כל הטקסט מתוך הקובץ ותחזיר אותו כמחרוזת.

##חקר עמוק:
פונקצית 'slurp' הוצגה בשפת Clojure מהגרסה 1.0. היא מבצעת פעולה אטומית, כלומר, היא תמיד תקרא את כל הקובץ לזיכרון לפני שתחזיר את המחרוזת.
חלופה לשימוש בפונקציה 'slurp' היא שימוש בפונקצית 'line-seq':
```Clojure
(with-open [r (reader "myfile.txt")]
  (doseq [line (line-seq r)]
    (println line)))
```
פונקצית 'line-seq' מאפשרת קריאת הקובץ לפי שורות, מה שיוצר כמה שיפור בביצועים במקרה שהקובץ הוא ענק.

## ראה גם:
1. [מדריך לקריאת דיקשנרי מקובץ טקסט](http://learn-clojure.com/read-dict-from-file)
2. [מסמך המסביר יותר מעמיקים את הפונקציה 'slurp'](https://clojure.org/guides/slurp)
3. [דיון על 'slurp' מול 'line-seq'](https://stackoverflow.com/questions/37992580/clojure-when-to-use-slurp-vs-line-seq)