---
date: 2024-01-26 04:33:09.066933-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E4\u05E2\u05E0\u05D5\u05D7, \u05E9\u05D0\u05D9\u05DC\u05EA\u05D5\u05EA\
  \ \u05D5\u05D4\u05EA\u05E2\u05E1\u05E7\u05D5\u05EA \u05D1\u05DE\u05E1\u05DE\u05DB\
  \u05D9 XML \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA Java. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05E9\
  \u05DD \u05D7\u05DC\u05D9\u05E4\u05D9\u05DF \u05E9\u05DC \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD, \u05E0\u05D9\u05D4\u05D5\u05DC \u05EA\u05E6\u05D5\u05E8\u05D4, \u05D5\
  \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05DE\u05E2\u05E8\u05DB\u05D5\u05EA\
  \ \u05D9\u05E9\u05E0\u05D5\u05EA \u05E8\u05D1\u05D5\u05EA \u05D5-APIs\u2026"
lastmod: '2024-03-13T22:44:39.172134-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E4\u05E2\u05E0\u05D5\u05D7, \u05E9\u05D0\u05D9\u05DC\u05EA\u05D5\u05EA\
  \ \u05D5\u05D4\u05EA\u05E2\u05E1\u05E7\u05D5\u05EA \u05D1\u05DE\u05E1\u05DE\u05DB\
  \u05D9 XML \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA Java."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
weight: 40
---

## איך לעשות:
Java מספקת APIs כמו DOM (Document Object Model), SAX (Simple API for XML), ו-StAX (Streaming API for XML) לעבודה עם XML. הנה דוגמה של DOM לפענוח קובץ XML:

```java
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class XmlParser {
    public static void main(String[] args) {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse("data.xml");
            
            doc.getDocumentElement().normalize();
            NodeList nodeList = doc.getElementsByTagName("employee");
            
            for (int i = 0; i < nodeList.getLength(); i++) {
                Element element = (Element) nodeList.item(i);
                System.out.println("Name: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Age: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

נניח ש-`data.xml` נראה כך:

```xml
<employees>
    <employee>
        <name>Jane Doe</name>
        <age>30</age>
    </employee>
    <employee>
        <name>John Doe</name>
        <age>40</age>
    </employee>
</employees>
```

הפלט יהיה:

```
Name: Jane Doe
Age: 30
Name: John Doe
Age: 40
```

## צלילה עמוקה
XML נמצא כאן מאז סוף שנות ה-90, ומספק דרך מובנת וגמישה לחליפין נתונים בין מערכות שונות. למרות ש-JSON הפך לפופולרי יותר עבור ה-Web APIs החדשים בזכות תחבירו הפשוט יותר והשילוב ההדוק עם JavaScript, XML עדיין בשימוש נרחב בסביבות ארגוניות, שירותי רשת מבוססי SOAP, ובתקני מסמכים כמו Office Open XML ל-Microsoft Office.

כאשר מדובר בפענוח XML ב-Java, ה-API של DOM מעולה למסמכים קטנים: הוא מבוסס על עץ ומאפשר גישה מלאה למבנה ה-XML בזיכרון. עם זאת, לקבצים גדולים יותר, זה יכול להיות דרוש זיכרון רב. SAX ו-StAX ידידותיים יותר לזיכרון, מכיוון שהם מבוססים על אירועים ועל זרם בהתאמה, אך הם יכולים להיות פחות נוחים לניווט במבני XML.

ליצירה או שינוי של XML, Java מספקת גם את החבילות javax.xml.transform ו-javax.xml.bind (JAXB). JAXB הייתה חלק מ-Java SE עד לגרסה 10, לאחר מכן, היא הפכה לספרייה נפרדת בשל הסרת מודולי Java EE מ-Java SE. זה דרך מונעת הערות לנרמול אובייקטי Java ל-XML ולהפך.

## ראה גם
בדוק את המקורות הקשורים הבאים למידע נוסף על עבודה עם XML ב-Java:
- [Java API עבור עיבוד XML (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java Architecture עבור XML Binding (JAXB)](https://javaee.github.io/jaxb-v2/)
- [מדריך Oracle ל-XML ב-Java](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [טכנולוגיה של XML של W3C](https://www.w3.org/standards/xml/)
- [Stack Overflow: שאלות עם התגים 'java' ו-'xml'](https://stackoverflow.com/questions/tagged/java+xml)
