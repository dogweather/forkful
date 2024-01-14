---
title:    "Kotlin: חילוץ תת מחרוזות"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה
יתרון ראשון של שימוש בחילוץ מחרוזות תחתיות הוא לאפשר לנו לסנן ולעבד נתונים מסוימים מתוך מחרוזות גדולות יותר. 

## איך לעשות זאת
תחילה, נכין מחרוזת מסוימת שנרצה לחלץ ממנה מחרוזת תת-תחתית. לדוגמה, אם נרצה לחלץ את המילה "יקירתי" מהמחרוזת "שלום יקירתי", נשתמש בפקודה "substring" כך: 

```Kotlin
val str = "שלום יקירתי"
val substr = str.substring(6) // substr will contain "יקירתי"
```

ניתן גם להשתמש בפקודה "substring" כדי לחלץ מחרוזת תת-תחתית על פי מיקום התווים במחרוזת. לדוגמה, אם נרצה לחלץ את המילה "גברת" מהמחרוזת "שלום גברת יקירתי", נשתמש בפעולה הבאה:

```Kotlin
val str = "שלום גברת יקירתי"
val substr = str.substring(5, 10) // substr will contain "גברת"
```

## למעמדי
בניגוד לפקודת "substring", הפקודה "subSequence" מחזירה אובייקט מסוג CharSequence (מחרוזת תת-תחתית), שבו ניתן לעבוד עם המחרוזת התת-תחתית בפני עצמה. כמו כן, ניתן לעבוד עם מחרוזות תת-תחתיות באמצעות פקודת "replace" כדי להחליף מחרוזת תת-תחתית עם מחרוזת אחרת.

## ראה גם
- [המדריך המלא לשפת קוטלין](https://www.codecademy.com/learn/learn-kotlin)
- [תיעוד רשמי לפקודת substring בשפת קוטלין](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [מדריך לשימוש בפקודת subSequence בשפת קוטלין](https://www.javatpoint.com/kotlin-subsequence)