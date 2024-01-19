---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

פרסונג HTML - זהו תהליך שישלוח קוד HTML, בחילופי מחרוזות טקסט לאובייקטי DOM (מודלי אובייקט של מסמכים). מתכנתים עוסקים בזה כדי ליצור, לתקן, או לנתח דפים אינטרנט.

## הדרך לעשות את זה:

באמצעות הספרייה ״Jsoup״, ניתן לקחת דף אינטרנט ולהמיר כמה שמירהם - לאובייקטים Kotlin.

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val doc = Jsoup.connect("http://example.com").get()
    val title = doc.title()
    println("Title: $title")
}
```
הפלט:
```Kotlin
Title: Example Domain
```

## צלילה עמוקה:

אז, בעבר, היינו משתמשים בניתוחות מבנים משלנו כדי לבצע את זה. אבל, זה היה מסורבל וקשה לתחזוקה. כאן נכנסת Jsoup, הספרייה שמצליחה לעזור לנו ולעשות את העבודה במקום כל זה! ניתוח HTML גם יכול להיעשות באמצעות הספרייה "html.parser" אך Jsoup היא אפשרות פופולרית יותר בגלל תמיכתה בDOM.

## ראה גם:

DbVisualizer: [www.dbvis.com/download/](https://www.dbvis.com/download/)
Kotlin Official Documentation: [kotlinlang.org/docs/reference/](https://kotlinlang.org/docs/reference/) 
Jsoup Documentation: [jsoup.org](https://jsoup.org) 
HTML parsing in Python: [www.crummy.com/software/BeautifulSoup/bs4/doc/](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)