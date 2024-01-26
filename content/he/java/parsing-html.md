---
title:                "ניתוח HTML"
date:                  2024-01-20T15:32:43.180547-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
(Parse HTML: מה זה ולמה עושים את זה?)
Parsing HTML בג'אווה זה פשוט לקרוא ולהבין קוד HTML באמצעות תוכנה. זה נחוץ למשל כשאתה רוצה למשוך מידע מדפי אינטרנט או לעבוד עם התוכן שלהם בתוך האפליקציה שלך.

## איך לעשות:
(דוגמאות קוד ופלט דוגמה)

עלינו להשתמש בספרייה שתעזור לנו בפענוח. נבחר ב-jsoup, ספרייה פופולרית לעבודה עם HTML בג'אווה.
```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class HTMLParser {
    public static void main(String[] args) {
        String html = "<html><head><title>First Parse</title></head>"
                    + "<body><p>Parsed HTML into a doc.</p></body></html>";
        Document doc = Jsoup.parse(html);
        Elements paragraphs = doc.select("p");
        
        for (Element paragraph : paragraphs) {
            System.out.println(paragraph.text());
        }
    }
}
```
פלט דוגמה:
```
Parsed HTML into a doc.
```

## צלילה לעומק:
(jsoup: פרטים נוספים)

ב-2006, פרויקט jsoup נולד לחיים, כדי להפוך פענוח HTML לקל ואינטואיטיבי. הוא מאפשר לנו לנתח HTML כפי שהוא מופיע בדפדפן, עם תחביר נוח ופשוט.

חלופות הן לדוגמא: HTMLCleaner, JsoupXpath ועוד.

בחירת ספרייה תלויה בצרכים: jsoup טובה לרוב המקרים, אבל אם יש צורך ב-XML למשל, ייתכן שנבחר בספריה אחרת.

## ראה גם:
(קישורים למקורות קשורים)

- [jsoup - הדוקומנטציה הרשמית](https://jsoup.org/)
- [מתכוני JSOUP](https://jsoup.org/cookbook/)
- [HTMLCleaner - ספרייה חלופית](http://htmlcleaner.sourceforge.net/)
- [Tutorial: Web scraping באמצעות Java ו-jsoup](https://www.baeldung.com/java-with-jsoup)
