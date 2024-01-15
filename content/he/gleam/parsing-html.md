---
title:                "ניתוח html"
html_title:           "Gleam: ניתוח html"
simple_title:         "ניתוח html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/parsing-html.md"
---

{{< edit_this_page >}}

# מדוע
אנו תמיד מגלים מידע חדש באינטרנט, וכאשר מתגוררים ליצור אפליקציות או אתרים, זה חשוב להבין ולהציג את המידע הנדרש בצורה ארגונית. לכן, לדעת איך לצרף תגי HTML יכול להיות מועיל מאוד עבור מפתחי אתרי אינטרנט ואפליקציות.

## איך לעשות זאת
קוד Gleam נועד לעזור לך להפעיל תהליך זה בצורה יעילה ופשוטה. כאן נמצא דוגמאות של כיצד לצרף תגי HTML באמצעות קוד Gleam.

```Gleam
import gleam/http
import gleam/html/parser as parser

pub fn parse_html(body: http.ResponseBody) {
    let doc = parser.parse(body)
    let title = doc % "title" |> parser.text
    let links = doc %% "a" |> parser.attr("href")
    let images = doc %% "img" |> parser.attr("src")
    let paragraphs = doc %% "p" |> parser.text
}
```

### הפלט הצפוי:

```Gleam
title: "כותרת האתר"
links: ["/about", "/contact", "/services"]
images: ["/image1.jpg", "/image2.jpg", "/image3.jpg"]
paragraphs: ["פרטים בנוגע לאתר", "כל הפרטים על הכתובת שלנו", "השירותים שאנו מציעים"]
```

## Deep Dive
כאשר משתמשים בקוד Gleam לצרף תגי HTML, כדאי לקחת בחשבון הבאים:
- יש לוודא שהתגייה היא חוקית ותואם ל-Standards הנכונים.
- ניתן לצרף את התגיות בכל מספר תוך כדי מציאת פרטי התוכן הטקסטואלי שלהם.
- יש לשמור את הקוד שגוף (body) עבור כל תגית יחידה.

## ראה גם
- [מדריך על השתמשות קוד Gleam לצרף תגי XML](https://gleam.run/documentation/guide/using-xml-code-gleam/)
- [דוגמאות על מנת להתחיל בשימוש קוד Gleam לתחילת דרך](https://gleam.run/documentation/getting-started/examples/)