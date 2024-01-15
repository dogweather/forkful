---
title:                "להתחיל פרויקט חדש"
html_title:           "Kotlin: להתחיל פרויקט חדש"
simple_title:         "להתחיל פרויקט חדש"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

# למה
כדי להתחיל פרויקט חדש באמצעות קוטלין ישנם מספר סיבות שעשויות למשוך ספקים. למשל, השפה היא בעלת תחביר פשוט וקריאה, מה שהופך את תנועה הליכה כדי ללמוד אותה.

# איך לעשות זאת
### קוד לדוגמה
כדי להתחיל פרויקט חדש בקוטלין, נצטרך להגדיר clssoes פרטיים שישמשו כצורת הפועל של הפרויקט. לדוגמה:

```Kotlin
class Person(val name: String, val age: Int)

val john = Person("John", 25)
println("Hi, my name is ${john.name} and I am ${john.age} years old.")
```
כאן, יצרנו class שמייצג איש ויצרנו אובייקט חדש בשם john עם שם וגיל. הקוד הבא מדפיס את המידע של john.

```
Hi, my name is John and I am 25 years old.
```

# חפירה עמוקה
כאשר מתחילים פרויקט חדש בקוטלין, חשוב להגדיר מטרות מדויקות ולבחון את הצרכים של הפרויקט. כמו כן, חשוב להתרגל לתנועת הלך המערכת וללמוד על פעולות וכלים נוספים שקוטלין מציעה כדי לפתור בעיות וליישם פתרונות מתאימים.

# ראו גם
- [המדריך המלא של קוטלין למתחילים](https://kotlinlang.org/docs/tutorials/getting-started.html)
- [פרויקטים מקודמים בקוטלין](https://kotlin.link/)