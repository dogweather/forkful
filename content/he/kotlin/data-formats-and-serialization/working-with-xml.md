---
date: 2024-01-26 04:34:24.413552-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 (Parsing), \u05D9\u05E6\u05D9\u05E8\u05D4\
  \ \u05D5\u05E9\u05D9\u05E0\u05D5\u05D9 \u05DE\u05E1\u05DE\u05DB\u05D9 XML - \u05E9\
  \u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF \u05DC\u05D0\u05D7\u05E1\u05D5\u05DF\
  \ \u05D5\u05D4\u05E2\u05D1\u05E8\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD. \u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05DE\u05E2\u05E8\u05DB\u05D5\u05EA\
  \ \u05E8\u05D1\u05D5\u05EA \u05E2\u05D3\u05D9\u05D9\u05DF \u05DE\u05D7\u05DC\u05D9\
  \u05E4\u05D5\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\u2026"
lastmod: '2024-03-11T00:14:12.762089-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 (Parsing), \u05D9\u05E6\u05D9\u05E8\u05D4\
  \ \u05D5\u05E9\u05D9\u05E0\u05D5\u05D9 \u05DE\u05E1\u05DE\u05DB\u05D9 XML - \u05E9\
  \u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF \u05DC\u05D0\u05D7\u05E1\u05D5\u05DF\
  \ \u05D5\u05D4\u05E2\u05D1\u05E8\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD. \u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05DE\u05E2\u05E8\u05DB\u05D5\u05EA\
  \ \u05E8\u05D1\u05D5\u05EA \u05E2\u05D3\u05D9\u05D9\u05DF \u05DE\u05D7\u05DC\u05D9\
  \u05E4\u05D5\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
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
