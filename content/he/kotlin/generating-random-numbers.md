---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
צורת ראנדומלית היא יצירת רצף של מספרים שאין קשר בין אחד לשני, או שאין דרך לחזות מה הם יהיו. מתכנתים מייצרים מספרים אקראיים כדי להגיע לתוצאות שונות במהלך חישובים, לבחינת תרחישים תחליפיים או להפגיש את המשתמש באפקט הלא צפוי. 

## איך לעשות את זה:
הנה דוגמה באמצעות Kotlin:
```
val randomValues = List(10) { Random.nextDouble() }
println(randomValues)
```
הקוד הפשוט הזה מייצר רשימה של 10 מספרים אקראיים בטווח של 0 עד 1.

## צלילה עמוקה:
תחביר ה-Kotlin מספק דרכים משופרות ליצירת מספרים אקראיים בהשוואה לשפות אחרות, כמו Java. זו נותנת לך את השליטה ליצור מספרים אקראיים בטווחים ספציפיים ובצורות מסוימות.
בנוסף לספריה הרגילה של Java, Kotlin מאפשר לך להשתמש במתודה Random.nextDouble(). המתודה הזו מחזירה מספר ממשי אקראי בטווח 0..1.

## ראו גם:
מדריך לדמיון אקראי ב-Kotlin:
https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/next-double.html

צורזת גייסון:
https://www.geeksforgeeks.org/generate-random-numbers-kotlin/