---
title:                "Clojure: המרת מחרוזת לאותיות קטנות"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

למה לכתוב פיסת קוד שתמיד יעביר סדרת תווים לאותיות קטנות?

## איך לעשות זאת

תגידו שאתם צריכים להעביר סדרת תווים לאותיות קטנות. כדי לעשות כך ב-Clojure, ניצור פונקציה פשוטה המשתמשת בפונקציות של השפה כדי לסדר את המחרוזת ולהחזיר את הסדרה הזו באותיות קטנות.

```clojure
(defn string-to-lower [s]
  (-> s
      (clojure.string/lower-case)))
```

בפונקציה הזו, אנו סופרים על השם `string-to-lower` שעושה שימוש בפונקציה `clojure.string/lower-case` המכילה את כל הפעולות הקשורות להמרת מחרוזת לאותיות קטנות.

לדוגמה, נזין את המחרוזת "HellO" לפונקציה ונקבל את המחרוזת "hello" כתוצאה.

```clojure
(string-to-lower "HellO") ; output: "hello"
```

## מעיין עמוק

כאשר אנו משתמשים בפונקציה `clojure.string/lower-case`, אנו משתמשים בפונקציות מעט יותר מתקדםות שמאפשרות לנו לבצע כל מיני פעולות על המחרוזת לפני שאנחנו ממירים אותה לאותיות קטנות.

לדוגמה, משתנים כמו `clojure.string/upper-case`, `clojure.string/capitalize` ו-`clojure.string/camel-case` יכולים לבצע טיפולים נוספים כמו המרת מחרוזת לאותיות גדולות, להשתמש באות גדולה בתחילת המשפט או להמיר אותיות לקטגוריית התבנית של "גמל גבולות".

## ראו גם

- [מדריך לפונקציות מחרוזת של Clojure](https://clojure.org/reference/strings)

- [תיעוד לפונקציות מחרוזת של Clojure](https://clojuredocs.org/clojure.string/lower-case)