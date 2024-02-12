---
title:                "עבודה עם XML"
aliases:
- /he/java/working-with-xml/
date:                  2024-01-26T04:33:09.066933-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML כוללת פענוח, שאילתות והתעסקות במסמכי XML באמצעות Java. מתכנתים עושים זאת לשם חליפין של נתונים, ניהול תצורה, ומכיוון שמערכות ישנות רבות ו-APIs מתקשרות באמצעות XML.

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
