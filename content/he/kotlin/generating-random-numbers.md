---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:49:29.990011-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה יצירת מספרים אקראיים ולמה זה חשוב בתכנות? יצירת מספרים אקראיים היא התהליך שבו אנו מייצרים ערכים לא צפויים. זה נחוץ למשחקים, בדיקות, אלגוריתמים של הסתברות ועוד.

## How to:
כדי ליצור מספרים אקראיים ב-Kotlin, אתה יכול להשתמש בקלאס `Random` או בפונקציות שכבר מובנות בסטנדרט לייבררי.

```Kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(0, 100) // מספר אקראי בין 0 ל-99
    println(randomNumber)
}
```

עכשיו הפלט יהיה מספר אקראי מדי פעם שהתוכנית תרוץ.

## Deep Dive
מיום המצאת המחשבים, הייתה צורך במספרים אקראיים לשימושים שונים כמו קריפטוגרפיה ומשחקים. כללית, יש שני סוגים של גנרטורים: פסאודו-אקראיים (PRNGs) והארדוור אקראי (TRNGs). ב-Kotlin, `Random` הוא PRNG, כלומר, עם אותו זרע יצירת המספרים ('seed'), הוא ייצר את אותה סדרה של מספרים. אם ברצונך מספר אקראי יותר "אמיתי", ייתכן שתרצה לשקול אפשרויות נוספות כמו גנרטורים המבוססים על אוסף נתונים אקראיים מהסביבה שלך.

## See Also
- המדריך הרשמי לכללים של אקראיות ב-Kotlin: [Kotlin Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/)
- עיון עמוק יותר בגנרטורים פסאודו-אקראיים: [PRNGs](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- רקע על השימוש בגנרטורים הארדוור אקראיים שהוא בסיסי לקריפטוגרפיה: [TRNGs](https://en.wikipedia.org/wiki/True_random_number_generator)