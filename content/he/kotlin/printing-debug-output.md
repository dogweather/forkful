---
title:                "Kotlin: הדפסת פלט מנקודת בדיקה"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## למה

העבודה עם קוד מסובכת ומתוחכמת יכולה להיות מאתגרת ולכן גורמת לשגיאות להשתגע. הדפסת תפוצה נשבעת או עלולה לסייע למתכנת לחפש את התוכן שלנו כדי להבין איפה התקדמנו ואיך הקוד עובד.

## כיצד לעשות זאת

עם קוד הקוד הבא, נוכל להדפיס הודעות בעת הפעלת התכנית:

```Kotlin
println("התכנית הופעלה.")
```

בכדי להוסיף מידע נוסף נוכל להשתמש " ${}" כדי לכלול משתנים בתוך הדפסות:

```Kotlin
val name = "שלום"
println("היי, שמי הוא $שם.")
```

## התפקדות עמוקה

הדפסת תפוצה היא כלי חשוב מאוד לבדיקת קוד ותיקונו. בנוסף לכך, ניתן להשתמש בפונקציות כמו `Log` ו-`Debug` כדי להדפיס הודעות ברמות שונות של הדפסה. זה יכול לסייע לנו לנתח את התהליך של הקוד ולאתר מחלקות או תכונות עם בעיות.

## ראו גם

- [כשלון בקוד: למה זה קורה ואיך לתקן את זה](https://www.example.com/hebrew/debugging-failures)
- [כיצד להשתמש בפונקציית `print()` בקוד שלנו](https://www.example.com/hebrew/using-print-function)