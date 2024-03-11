---
date: 2024-01-26 00:55:34.381768-07:00
description: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05D4\u05D5\u05D0 \u05D0\u05D5\u05E4\u05DF \u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\
  \u05D5\u05EA \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA \u05E2\u05DD \u05D1\u05E2\
  \u05D9\u05D5\u05EA \u05E9\u05E2\u05D5\u05DC\u05D5\u05EA \u05D1\u05DE\u05D4\u05DC\
  \u05DA \u05D4\u05E8\u05D9\u05E6\u05D4 - \u05DB\u05DE\u05D5 \u05DC\u05D4\u05EA\u05DE\
  \u05D5\u05D3\u05D3 \u05E2\u05DD \u05DB\u05D3\u05D5\u05E8 \u05E2\u05E7\u05D5\u05DD\
  \ \u05DC\u05DC\u05D0 \u05D4\u05D7\u05DE\u05E6\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05DE\u05E0\u05D5\u05E2 \u05E7\u05E8\u05D9\u05E1\u05D5\u05EA \u05D5\u05DC\
  \u05EA\u05EA\u2026"
lastmod: '2024-03-11T00:14:12.733138-06:00'
model: gpt-4-1106-preview
summary: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05D4\u05D5\u05D0 \u05D0\u05D5\u05E4\u05DF \u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\
  \u05D5\u05EA \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA \u05E2\u05DD \u05D1\u05E2\
  \u05D9\u05D5\u05EA \u05E9\u05E2\u05D5\u05DC\u05D5\u05EA \u05D1\u05DE\u05D4\u05DC\
  \u05DA \u05D4\u05E8\u05D9\u05E6\u05D4 - \u05DB\u05DE\u05D5 \u05DC\u05D4\u05EA\u05DE\
  \u05D5\u05D3\u05D3 \u05E2\u05DD \u05DB\u05D3\u05D5\u05E8 \u05E2\u05E7\u05D5\u05DD\
  \ \u05DC\u05DC\u05D0 \u05D4\u05D7\u05DE\u05E6\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05DE\u05E0\u05D5\u05E2 \u05E7\u05E8\u05D9\u05E1\u05D5\u05EA \u05D5\u05DC\
  \u05EA\u05EA\u2026"
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
טיפול בשגיאות הוא אופן התמודדות הקוד שלך עם בעיות שעולות במהלך הריצה - כמו להתמודד עם כדור עקום ללא החמצה. מתכנתים עושים זאת כדי למנוע קריסות ולתת למשתמשים חווית שימוש חלקה.

## איך לעשות:
קוטלין מספקת את המילים השמורות `try`, `catch`, `finally` ו- `throw` כדי לנהל שגיאות. וכך אתה משתמש בהם:

```Kotlin
fun main() {
    val numerator = 10
    val denominator = 0

    try {
        val result = numerator / denominator
        println("תוצאה: $result")
    } catch (e: ArithmeticException) {
        println("אי אפשר לחלק באפס, חבר.")
    } finally {
        println("זה קורה לא משנה מה.")
    }
}
```

פלט:
```
אי אפשר לחלק באפס, חבר.
זה קורה לא משנה מה.
```

אם משהו לא תקין קורה בבלוק ה-`try`, הביצוע מתחיל לדלג ל-`catch`. הוא תופס את השגיאה הספציפית שנזרקה (במקרה זה `ArithmeticException`). הבלוק `finally` רץ לאחר מכן - לא משנה מה התוצאה.

## טבילה עמוקה
בלוק `try-catch` הוא דבר מימי התוכנות המוקדמים - זה כמו רשת ביטחון. קוטלין גם מציעה את `throw` לזריקת חריגה באופן ידני לרינג, ויש את `finally` לקוד שחייב לרוץ - עבודות ניקוי, לרוב.

חלופות כוללות את סוג `Result` ואת קוטלין `try` בתור ביטוי.

```Kotlin
val result: Result<Int> = try {
    Result.success(numerator / denominator)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
גישה זו מחזירה אובייקט `Result` - אתה מקבל או הצלחה או כישלון ללא הדרמה של חריגה שלא טופלה.

היישום בקוטלין הוא נקי כי ניתן להשתמש ב-`try` כביטוי, כלומר היא מחזירה ערך. אופציות כאלה הופכות את טיפול בשגיאות בקוטלין למגוון למדי. זה עניין של לבחור את הכלי הנכון לעבודה, בדיוק כמו שהיית עושה בסדנא.

## ראה גם
- מסמכי קוטלין על חריגים: [טיפול בחריגות בקוטלין](https://kotlinlang.org/docs/exception-handling.html)
- מסמכי סוג `Result` של קוטלין: [תוצאת קוטלין](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- Effective Java, מהדורה שלישית, מאת ג'ושוע בלוך - תובנות נהדרות על חריגים, למרות שהוא ספציפי לג'אווה.
