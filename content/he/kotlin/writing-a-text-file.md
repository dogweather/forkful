---
title:                "Kotlin: כתיבת קובץ טקסט"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה

כתיבת קובץ טקסט היא כלי חשוב בתכנות ב-Kotlin שימושי להכנסת מידע ושמירת פרטים משתנים לשימוש עתידי. כתיבת טקסט גם עשוייה להימשך יותר מכתיבת קוד ולכן יתכן שהיא תהיה אפשרות נוחה יותר לשימוש עבור מתכנתים.

## איך לעשות

כדי לכתוב קובץ טקסט ב-Kotlin, ניתן להשתמש בפעולת `writeText()` ליצירה וכתיבת קובץ טקסט חדש. לדוגמה, ננסה ליצור קובץ טקסט ולכתוב בתוכו את המילים "שלום עולם":

```Kotlin
val file = File("textfile.txt")
file.writeText("שלום עולם")
```

הפעולה `writeText()` תיצור את הקובץ החדש אם הוא לא קיים ותחליף את התוכן הקיים שלו אם הוא קיים כבר. לאחר ההרצה, נוכל לראות בתוכן הקובץ `textfile.txt` את המילים "שלום עולם".

## Deep Dive

כלים נוספים עבור כתיבת קבצי טקסט כוללים את פעולת `appendText()` שמאפשרת להוסיף תוכן לקובץ קיים במקום להחליף אותו, ופעולת `writeBytes()` שמאפשרת כתיבה של מערך בתים לתוך קובץ.

## ראה גם

למידע נוסף על פעולות של כתיבת קבצים ב-Kotlin, ניתן לקרוא בנושא באתר הרשמי של Kotlin כאן:
https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/write-text.html

לדוגמאות נוספות על שימוש ב-Kotlin כדי לכתוב קבצי טקסט, ניתן לצפות בסרטוני הלימוד המצולמים של Kotlin פה:
https://kotlinlang.org/docs/reference/coding-conventions.html#documentation-comments