---
date: 2024-01-20 15:32:36.546745-07:00
description: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML \u05D4\u05D5\u05D0 \u05EA\u05D4\
  \u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E7\u05D5\
  \u05E8\u05D0\u05D9\u05DD \u05D5\u05DE\u05E9\u05D9\u05D2\u05D9\u05DD \u05DE\u05D9\
  \u05D3\u05E2 \u05DE\u05DE\u05E1\u05DE\u05DB\u05D9 HTML. \u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05D0\u05E1\u05D5\u05E3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D1\
  \u05E6\u05E2 \u05D0\u05D5\u05D8\u05D5\u05DE\u05E6\u05D9\u05D4 \u05D0\u05D5 \u05DC\
  \u05D1\u05E0\u05D5\u05EA \u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D5\u05EA\
  \ \u05E9\u05E4\u05D5\u05E0\u05D5\u05EA \u05DC\u05DE\u05DE\u05E9\u05E7\u05D9 \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05E0\u05D8."
lastmod: '2024-02-25T18:49:37.508219-07:00'
summary: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML \u05D4\u05D5\u05D0 \u05EA\u05D4\
  \u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E7\u05D5\
  \u05E8\u05D0\u05D9\u05DD \u05D5\u05DE\u05E9\u05D9\u05D2\u05D9\u05DD \u05DE\u05D9\
  \u05D3\u05E2 \u05DE\u05DE\u05E1\u05DE\u05DB\u05D9 HTML. \u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05D0\u05E1\u05D5\u05E3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D1\
  \u05E6\u05E2 \u05D0\u05D5\u05D8\u05D5\u05DE\u05E6\u05D9\u05D4 \u05D0\u05D5 \u05DC\
  \u05D1\u05E0\u05D5\u05EA \u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D5\u05EA\
  \ \u05E9\u05E4\u05D5\u05E0\u05D5\u05EA \u05DC\u05DE\u05DE\u05E9\u05E7\u05D9 \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05E0\u05D8."
title: "\u05E0\u05D9\u05EA\u05D5\u05D7 HTML"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
פיענוח HTML הוא תהליך שבו אנחנו קוראים ומשיגים מידע ממסמכי HTML. תכנתים עושים זאת כדי לאסוף נתונים, לבצע אוטומציה או לבנות אפליקציות שפונות לממשקי אינטרנט.

## How to: (איך לעשות:)
בואו נראה איך עובדים עם ביבליות פיענוח בKotlin.
שימו לב, אנחנו משתמשים בספריית Jsoup כאן:

```kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>First parse</title></head>"
              + "<body><p>Parsed HTML into a doc.</p></body></html>"
    val doc = Jsoup.parse(html)
    val title = doc.title()
    val pText = doc.select("p").first().text()

    println("Title: $title")
    println("Paragraph: $pText")
}
```

פלט דוגמה:
```
Title: First parse
Paragraph: Parsed HTML into a doc.
```

זה כל מה שצריך כדי להתחיל לקרוא נתוני HTML ולמצוא אלמנטים בעזרת סלקטורים.

## Deep Dive (צלילה עמוקה)
נתחיל בקצת היסטוריה. בשנות ה-90, כשהאינטרנט התחיל לתפוס תאוצה, עיבוד HTML היה מורכב ורדוד. לא היו סיפריות וקוד היה רגיש לשגיאות. כיום, יש לנו ספריות עשירות כמו Jsoup בJava וKotlin שנותנות לנו כוח לעבד HTML בקלות ויעילות.

חלופות לJsoup כוללות HtmlUnit או TagSoup, אך Jsoup היא המועדפת מכיוון שהיא ברורה, עקבית וקלה לשימוש.

ברמה טכנית, פיענוח HTML עם Jsoup דורש ממיר את המסמךString ל-Document עם הפונקציה `parse`, ואז אפשר להשתמש בשיטות של DOM (Document Object Model) לדליפת המידע הרצוי.

## See Also (ראו גם)
- [Jsoup Documentation](https://jsoup.org/cookbook/)
- [Kotlin Programming Language](https://kotlinlang.org/)
- [HtmlUnit](http://htmlunit.sourceforge.net/)
