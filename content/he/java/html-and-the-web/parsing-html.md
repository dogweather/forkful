---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:06.839223-07:00
description: "\u05E4\u05E2\u05E0\u05D5\u05D7 HTML \u05DE\u05E9\u05DE\u05E2\u05D5 \u05DC\
  \u05D7\u05E4\u05E9 \u05D1\u05E2\u05D5\u05DE\u05E7 \u05D4\u05E1\u05D9\u05DE\u05D5\
  \u05DF \u05DB\u05D3\u05D9 \u05DC\u05D7\u05DC\u05E5 \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05DB\u05DE\u05D5 \u05D8\u05E7\u05E1\u05D8, \u05E7\u05D9\u05E9\u05D5\u05E8\
  \u05D9\u05DD, \u05D0\u05D5 \u05D0\u05DC\u05DE\u05E0\u05D8\u05D9\u05DD \u05D0\u05D7\
  \u05E8\u05D9\u05DD. \u05D0\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E4\u05E9\u05E8 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E7\u05E6\u05D9\u05D4 \u05E2\u05DD \u05EA\u05D5\u05DB\u05DF \u05D0\u05D9\
  \u05E0\u05D8\u05E8\u05E0\u05D8\u05D9 \u05D0\u05D5 \u05DC\u05E6\u05E8\u05E3 \u05D0\
  \u05D5\u05EA\u05D5,\u2026"
lastmod: '2024-03-11T00:14:12.568940-06:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05E2\u05E0\u05D5\u05D7 HTML \u05DE\u05E9\u05DE\u05E2\u05D5 \u05DC\
  \u05D7\u05E4\u05E9 \u05D1\u05E2\u05D5\u05DE\u05E7 \u05D4\u05E1\u05D9\u05DE\u05D5\
  \u05DF \u05DB\u05D3\u05D9 \u05DC\u05D7\u05DC\u05E5 \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05DB\u05DE\u05D5 \u05D8\u05E7\u05E1\u05D8, \u05E7\u05D9\u05E9\u05D5\u05E8\
  \u05D9\u05DD, \u05D0\u05D5 \u05D0\u05DC\u05DE\u05E0\u05D8\u05D9\u05DD \u05D0\u05D7\
  \u05E8\u05D9\u05DD. \u05D0\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E4\u05E9\u05E8 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E7\u05E6\u05D9\u05D4 \u05E2\u05DD \u05EA\u05D5\u05DB\u05DF \u05D0\u05D9\
  \u05E0\u05D8\u05E8\u05E0\u05D8\u05D9 \u05D0\u05D5 \u05DC\u05E6\u05E8\u05E3 \u05D0\
  \u05D5\u05EA\u05D5,\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
---

{{< edit_this_page >}}

## מה ולמה?

פענוח HTML משמעו לחפש בעומק הסימון כדי לחלץ נתונים כמו טקסט, קישורים, או אלמנטים אחרים. אנו עושים זאת כדי לאפשר אינטרקציה עם תוכן אינטרנטי או לצרף אותו, לאוטמט פעולות גלישה או לבדוק אפליקציות אינטרנט.

## כיצד:

בואו נשתמש ב-Jsoup, ספרייה נוחה לעבודה עם HTML מהעולם האמיתי. קודם כל, הוסיפו את התלות:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

עכשיו לחלק המהנה. הנה איך לתפוס את כותרת הדף של אתר אינטרנט ולהדפיסה:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParser {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String title = doc.title();
        System.out.println("Title: " + title);
    }
}
```

פלט:

```
Title: Example Domain
```

מה עם חילוץ כל הקישורים?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... בתוך ה-main או שיטה אחרת
Elements links = doc.select("a[href]");
for (Element link : links) {
    System.out.println("Link: " + link.attr("href"));
}
```

## צלילה עמוקה

פעם, HTML היה מנוהל על ידי תבניות regex, שיטה שהייתה מוטעית ומפלצתית עבור מסמכים מורכבים. אז הגיע Jsoup בסוף שנות האפס, והציע ממשק דמוי-jQuery עבור Java לפרסול, ניווט, וניהול HTML.

Jsoup אינו הבחירה היחידה. ישנו HtmlUnit לבדיקות אפליקציות אינטרנט בסדר גודל מלא עם תמיכה ב-JavaScript, אבל הוא כבד יותר ומורכב יותר. למשימות קלות, Apache Commons Validator מעולה רק לחילוץ כתובות URL.

מאחורי הקלעים, Jsoup משתמש במפענח DOM, שמדגם את כל המסמך בזיכרון כעץ. גישה זו הופכת את הבחירה והניווט במבנה ה-HTML לקלים במיוחד. עוד, הוא סובלני עם HTML לא מסודר, תוקן בעיות במהלך העבודה כדי להבטיח פרסול אמין.

זכרו, כשאתם צורפים תוכן, תמיד בדקו את קובץ ה`robots.txt` של האתר ואת תנאי השירות כדי למנוע צרות משפטיות או חסימת כתובת ה-IP שלכם.

## ראו גם

- מסמכי Jsoup הרשמיים: https://jsoup.org/
- HtmlUnit: http://htmlunit.sourceforge.net/
- Apache Commons Validator: https://commons.apache.org/proper/commons-validator/
