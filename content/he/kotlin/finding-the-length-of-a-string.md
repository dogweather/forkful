---
title:    "Kotlin: מציאת אורך של מחרוזת"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# למה

למה לחפש את אורך המחרוזת? בשפת קוד החדשה והמתקדמת של Kotlin, מחרוזות הן אחת המתאפיינות הנפוצות והחשובות ביותר. בדרך כלל נבצע עליהם פעולות רבות, לכן חשוב לדעת את אורךן על מנת להתאים את הקוד לצרכים המדוייקים שלנו.

# כיצד לעשות זאת

בשפת Kotlin ישנן מספר דרכים שונות למציאת אורך המחרוזת. הנה כמה דוגמאות נפוצות:

```kotlin
// מחרוזת רגילה
val str= "שלום"
println(str.length) // תוצאה יהיה 4

// מחרוזת מכילה רווחים ותווים מיוחדים
val str2= "Hello, world!"
println(str2.length) // תוצאה תהיה 13 כולל גם את הרווחים והסימנים המיוחדים

// מחרוזת מורכבת
val str3= "😊 💻 📚"
println(str3.length) // תוצאה תהיה 3, גם כאן נכללים את כל התווים המיוחדים
```

לכולם תהיה תוצאה שונה בהתאם למרכיבי המחרוזת שונים שלהם.

# חקירה עמוקה

איך הקוד עובד בדיוק כדי למצוא את אורך המחרוזת? בפנים, הפונקציה length מחזירה את המספר הכללי של התווים במחרוזת, כולל תווים מיוחדים ורווחים. מהווה היתרון של השימוש בפונקציה זו שהיא עובדת עבור כל סוגי המחרוזות ולא רק נקודתיים או גדולים.

# ראה גם

- [מדריכי Kotlin בעברית](https://kotlin-il.com/)
- [ספריית הלימוד הרשמית של Kotlin] (https://kotlinlang.org/docs/home.html)
- [פורום הדיון הרשמי של Kotlin] (https://discuss.kotlinlang.org/)