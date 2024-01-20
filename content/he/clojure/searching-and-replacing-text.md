---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפה של טקסט היא תהליך של מציאת מחרוזות של טקסט מסוימות ושינויים שלהם לתוך משהו אחר. מתכנתים מבצעים אותו כדי לשנות נתונים, לשפר קריאות או לבצע תיקונים באופן אוטומטי.

## איך לעשות:
הנה דוגמה לחיפוש והחלף באמצעות Clojure: 

```Clojure
(defn search-and-replace [s old new]
  (clojure.string/replace s old new))

(defn -main []
  (println (search-and-replace "Hello, World!" "World" "Clojure")))
```

בפעילות זו, התרגיל מחליף את "World" ב"Hello, World!" עם "Clojure". הפלט שמודפס הוא:

```Clojure
Hello, Clojure!
```

## צלילה עמוקה 
חיפוש והחלפה של טקסט הם הליכים בסיסיים שהם חלק מזמן מה של עבודת מתכנת. הם נמנים עם הכלים הראשונים שהתפתחו בשפות תכנות. Clojure, שהייתה הראשונה בשנת 2007, משתמשת בפונקציית המילואים clojure.string/replace לתמיכה זו.
 
יחד עם זאת, ישנם אלטרנטיבות רבות אחרות לפונקצייה זו. לדוג' sets ו-maps הם דרך טובה נוספת לבצע חיפושים והחלפות, תוך הקפת הצורך באופן בו clojure.string/replace פועלת. 

נתינת קליטה עמוקה לפרטים הטכניים, clojure.string/replace משתמשת באלגוריתם של חיפוש והחלפה פשוט. בתחילה, הוא מחפש את המטבע המיוחל במחרוזת. לאחר מכן, כאשר מוצא את זה, הוא מחליף אותה.

## ראה גם 
[החלפת מחרוזת ב-Clojure](https://clojuredocs.org/clojure.string/replace) 
[חיפוש והחלפה של טקסט: פתרון עקיפה פשוט](https://clojure.org/guides/learn/functional_data_structures)