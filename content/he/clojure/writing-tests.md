---
title:    "Clojure: כתיבת בדיקות"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## למה

כתיבת טסטים היא חלק חשוב מהפיתוח בשפת Clojure. הטסטים מאפשרים לנו לבדוק את הקוד שכתבנו ולוודא שהוא עובד בדיוק כפי שנדמה לנו. הם מאפשרים לנו למצוא באופן מהיר ויעיל באיזה חלקים של הקוד יש תקלות ולתקן אותם לפני שהם מפריעים לקוד הכללי.

## איך לעשות זאת

השתמשו בקוד `clojure.test` כדי לכתוב טסטים בשפת Clojure. הטסטים יושבים בתיקיית `test` ובתוך כל קובץ יש להשתמש בפונקציות `deftest` כדי להגדיר טסט חדש ובנוסף לפעולות `is` ו-`are` כדי לעשות את הבדיקות. הנה דוגמאות של כתיבת טסטים בשפת Clojure:

```Clojure
(ns my-project.core-test
  (:require [clojure.test :refer :all]
            [my-project.core :refer :all]))

(deftest test-add
  (is (= 4 (add 2 2))))

(deftest test-multiply
  (are [x y]
      (= (* x y) (multiply x y))
      3 4
      5 5))
```

כאשר מריצים את הטסטים, נקבל תוצאות כמו הבאות:

```
lein test

my-project.core-test

Ran 2 tests containing 3 assertions.
0 failures, 0 errors.
```

## מעמקים

כדי לכתוב טסטים בצורה מקצועית, כדאי להכיר כל מה שאפשר לעשות עם הספרייה `clojure.test`. בנוסף, כדאי ללמוד על טכניקות כתיבת טסטים טובות כדי להבטיח שהטסטים שלנו מכסים את כל התרחישים האפשריים ומתפקדים בצורה אמינה.

## ראו גם

- [מדריך לכתיבת טסטים בשפת Clojure](https://www.learn-clojure.com/clojure-testing/)
- [מסמך רשמי על הספרייה `clojure.test`](https://clojure.github.io/clojure/clojure.test-api.html)
- [כתבי בלוג נוספים על כתיבת טסטים בשפת Clojure](https://clojuredocs.org/guides/testing)