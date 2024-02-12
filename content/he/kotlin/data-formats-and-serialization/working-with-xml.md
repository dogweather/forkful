---
title:                "עבודה עם XML"
aliases:
- /he/kotlin/working-with-xml.md
date:                  2024-01-26T04:34:24.413552-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML כוללת ניתוח (Parsing), יצירה ושינוי מסמכי XML - שפת סימון לאחסון והעברת נתונים. תכנתים עושים זאת מכיוון שמערכות רבות עדיין מחליפות נתונים בפורמט XML, וזה נדרש לתמיכה במערכות ישנות ואינטגרציה עם טכנולוגיות קיימות.

## איך לעשות:
ב-Kotlin, ניתן להשתמש ב-`javax.xml.parsers` המובנה לניתוח:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
ליצירת מסמכי XML, כדאי להשתמש ב-`javax.xml.transform`:

```Kotlin
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.Document
import java.io.StringWriter

fun convertDocumentToString(doc: Document): String {
    val transformer = TransformerFactory.newInstance().newTransformer()
    val result = StringWriter()
    transformer.transform(DOMSource(doc), StreamResult(result))
    return result.toString()
}
```
דוגמא לפלט של המרת מסמך למחרוזת תהיה פשוטה התוכן XML שלך בפורמט מחרוזת.

## צלילה עמוקה
XML היא אבן פינה של פיתוח אתרים ותוכנה משנות ה-90, שנבחרה בזכות הקריאות שלה והיררכיה המבנית. למרות ש-JSON זכה לפופולריות לשירותי אינטרנט בשל הפשטות וגודל ההודעה הקטן יותר שלו, XML נשאר נפוץ בסביבות ארגוניות, שירותי אינטרנט מבוססי SOAP, ותצורות (כמו קבצי פריסה של אנדרואיד).

ישנן ספריות ו-APIs שונים מעבר לתכונות המובנות של Kotlin/Java לטיפול ב-XML, כמו Simple XML Serialization ו-Jackson XML module. אבל `javax.xml.parsers` ו-`javax.xml.transform` בדרך כלל מספקות את רוב הצרכים מבלי להוסיף תלות חיצונית.

כאשר עוסקים ב-XML ב-Kotlin, פרטי היישום המרכזיים כוללים טיפול נכון בקידוד תווים וניהול ישויות XML כדי למנוע התקפות זריקת XML. יש להיות מודעים למורכבויות של מרחבי שמות ואימות סכימה בעת ניתוח XML כדי להבטיח את שלמות הנתונים.

## ראה גם
- [תיעוד Kotlin](https://kotlinlang.org/docs/reference/)
- [תיעוד Java DOM](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [מודול XML של Jackson](https://github.com/FasterXML/jackson-dataformat-xml)
