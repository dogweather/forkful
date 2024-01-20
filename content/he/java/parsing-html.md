---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ HTML הוא תהליך שבו אנו מפרקים קובץ HTML לישויות יחידות לניתוח נוסף. התכנתים עושים זאת שמצלבים באפקטיביות נתונים ממרחב האינטרנט.

## איך לעשות:
נסתכל על דוגמא ב-Java באמצעות Jsoup, הספריה הפופולרית ביותר לניתוח HTML.

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class ParseHtmlExample {
    public static void main(String[] args) {
        String html = "<html><body><p>Hello, wolrd!</p></body></html>";
        Document document = Jsoup.parse(html);
        Elements paragraphs = document.select("p");

        for (Element paragraph : paragraphs) {
            System.out.println(paragraph.text());
        }
    }
}
```
התוצאה תהיה:
```
Hello, world!
```

## הצצה לתובנה 
עם ההתפתחות של האינטרנט והידע המשותף, הוא לקח איתו סיפור היסטורי של חילוץ HTML. למרות שישנם דרכים אחרות לניתוח HTML - כמו XPath ו-DOM Parsing, Jsoup מספק את הקלות של jQuery מהסקריפטים של משתמש כדי לסננת את האיברים שאנו מחפשים. הוא מעצב להיות הכי יעיל ולעמוד באתגרים של ניתוח HTML.

## ראו גם
- [תיעוד Jsoup](https://jsoup.org/)
- [XPath](https://www.w3schools.com/xml/xpath_intro.asp)