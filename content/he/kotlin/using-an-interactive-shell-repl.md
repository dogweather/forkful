---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
date:                  2024-01-26T04:16:00.817414-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"

category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## מה ולמה?
REPL (Read-Eval-Print Loop - לולאת קריאה-חישוב-הדפסה) היא סביבת תכנות אינטראקטיבית ופשוטה. מתכנתים משתמשים בה לניסויי קוד מהירים, בדיקת קטעי קוד, או למידת תחביר שפה בלי ליצור אפליקציה מלאה.

## איך לעשות זאת:
להפעיל את REPL של Kotlin זה קל מאוד. פתחו את הטרמינל שלכם והקלידו `kotlinc`. אתם תגיעו לשלל של Kotlin. בואו ננסה להגדיר משתנה ולהדפיס את ערכו:

```kotlin
Welcome to Kotlin version 1.7.10 (JRE 1.8.0_292-b10)
Type :help for help, :quit for quit
>>> val greeting = "שלום, REPL של Kotlin!"
>>> println(greeting)
שלום, REPL של Kotlin!
```

## צלילה עמוקה
REPL של Kotlin הוצג לראשונה עם השפה עצמה כדי לעודד ניסויים. הוא דומה לשלל האינטראקטיבי של Python אך מותאם עבור תחביר ומיוחדויות של Kotlin. אלטרנטיבות? סביבות אינטראקטיביות בסביבות פיתוח כמו IntelliJ IDEA, ואזורי משחק של Kotlin באינטרנט. ה-REPL פועל על ידי הידור קוד "על המעוף", מה שמספק משוב מיידי – חשוב למידה ולאיתור באגים.

## ראו גם
- התיעוד של Kotlin על REPL: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- נסו את Kotlin בדפדפן: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- תוסף Kotlin Playground של JetBrains ל-IntelliJ IDEA.
