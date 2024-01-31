---
title:                "ניתוח HTML"
date:                  2024-01-20T15:32:36.546745-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/parsing-html.md"
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
