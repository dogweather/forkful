---
title:                "Kotlin: פריסת html"
simple_title:         "פריסת html"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## למה

מגרעת HTML היא תהליך חיוני עבור מפתחי אתרים ויישומי רשת. זה כולל קריאה וניתוח של קוד HTML כדי לייצר מבנה נתונים ולהציגו בצורה ידידותית. בעזרת Kotlin, אתה יכול ליצור כלי חזק וקל לשימוש כדי לפענח מנתח HTML בצורה מהירה ויעילה.

## כיצד לעשות זאת

הנה דוגמאות של כיצד להשתמש בקוד Kotlin כדי לפענח ולנתח HTML:

```Kotlin
// ייבוא המודולים הנדרשים
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

// URL של האתר שברצונך לפענח
val url = "https://www.example.com"

// יצירת חיבור לדף HTML של האתר
val doc = Jsoup.connect(url).get()

// קריאה והצגת כל הטקסט בתוך ה body של HTML
println(doc.body().text())

// ניתוח והצגת תגיות מסוימות על ידי השתמשות במזהה או מחרוזת תואמת
println(doc.select("#title").text()) // הצגת תוכן של תגית עם המזהה "כותרת"
println(doc.select("a[href]").attr("href")) // הצגת את הקישורים (href) בדף

// גילוי אוטומטי והצגת תוכן התגית הראשית בדף
println(doc.title())
```

פלט של מזהה התגית "כותרת" הצוהק: 

```
זהו כותרת הדף
```

פלט של המידע שנמצא תחת קישורים ה- href: 

```
https://www.example.com קישור לראות את האתר
```

## חקירה מעמיקה

על מנת להבין טוב יותר את מנגנוני פענוח HTML בעזרת Kotlin, ניתן להשתמש במבנה הדף Jsoup המקביל. ישנם מתודות שונות שניתן להשתמש בהן כדי לנתח את מבנה HTML בצורה יעילה וקלה לשימוש. שימוש במתודה `get()` תגרום ל-Jsoup להזמין את כל תוכן הדף הנוכחי באמצעות חיבור HTTP. ולאחר מכן, אתה תמונת את התוכן