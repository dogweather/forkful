---
title:                "Clojure: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## למה

למה כדאי לדאוג ליצור מספרים אקראיים בקוד שלנו? קוד המשתמש במספרים אקראיים יכול לגרום לתוצאות מגוונות יותר ולשפר את החוויה של המשתמשים שלנו.

## איך לעשות את זה

נתחיל עם דוגמא פשוטה של קוד Clojure ליצירת מספר אקראי:

```Clojure
(import java.util.Random)

(defn generate-random []
  (rand-int 100))

(def random-number (generate-random))

(println "המספר האקראי הוא: " random-number)
```

בקוד זה, אנו משתמשים בפונקציית `rand-int` המקבלת כפרמטר מספר ומחזירה מספר אקראי בתחום של 0 עד המספר הנתון (במקרה זה, 100). בתוך הקוד אנו יוצרים פונקציה בשם `generate-random` שמחזירה מספר אקראי באמצעות `rand-int`. לבסוף, אנו מדפיסים את המספר האקראי בעזרת `println`.

נסתכל עכשיו על דוגמא נוספת של קוד Clojure שמשתמשת בפונקציית `rand` ליצירת מספר אקראי בין 0 ל-1:

```Clojure
(rand)
```

פונקציית `rand` מחזירה מספר אקראי בין 0 ל-1 עם נקודה עשרונית. ניתן להציג יותר ספציפיות על ידי שימוש בפונקציית `format` כדי להגדיר את המספר המקסימלי שאנו רוצים לקבל:

```Clojure
(defn generate-random-decimal []
  (format "%.2f" (rand 10)))

(def random-decimal (generate-random-decimal))

(println "המספר האקראי עם שתי ספרות אחרי הנקודה הוא: " random-decimal)
```

בקוד זה, אנו משתמשים בפונקציית `format` כדי להגדיר את המספר המקסימלי ל-10. כך, המספר האקראי שנוצר יהיה בין 0 ל-10 עם שתי ספרות אחרי הנקודה.

## נחקר את העומק

כעת שימוש במספרים אקראי