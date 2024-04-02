---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:13.729732-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05D1-Google Apps Script\
  \ \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DC\u05E0\u05EA\u05D7, \u05DC\u05E2\u05D1\u05D3 \u05D5\u05DC\u05D9\u05D9\
  \u05E6\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9 XML, \u05D3\u05D1\u05E8 \u05D4\u05DB\
  \u05E8\u05D7\u05D9 \u05DC\u05E9\u05D9\u05E8\u05D5\u05EA\u05D9 \u05E8\u05E9\u05EA\
  \ \u05D5\u05D4\u05D2\u05D3\u05E8\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05D0\u05D5\u05DE\u05E6\u05D9\u05DD \u05D2\u05D9\u05E9\u05D4 \u05D6\u05D5\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DC\u05D1 \u05E2\u05DD \u05DE\u05E2\u05E8\u05DB\
  \u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.606620-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05D1-Google Apps Script\
  \ \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DC\u05E0\u05EA\u05D7, \u05DC\u05E2\u05D1\u05D3 \u05D5\u05DC\u05D9\u05D9\
  \u05E6\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9 XML, \u05D3\u05D1\u05E8 \u05D4\u05DB\
  \u05E8\u05D7\u05D9 \u05DC\u05E9\u05D9\u05E8\u05D5\u05EA\u05D9 \u05E8\u05E9\u05EA\
  \ \u05D5\u05D4\u05D2\u05D3\u05E8\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05D0\u05D5\u05DE\u05E6\u05D9\u05DD \u05D2\u05D9\u05E9\u05D4 \u05D6\u05D5\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DC\u05D1 \u05E2\u05DD \u05DE\u05E2\u05E8\u05DB\
  \u05D5\u05EA\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
weight: 40
---

## מה ולמה?

עבודה עם XML ב-Google Apps Script מאפשרת למתכנתים לנתח, לעבד ולייצר נתוני XML, דבר הכרחי לשירותי רשת והגדרות. מתכנתים אומצים גישה זו כדי לשלב עם מערכות ישנות, לבצע גריפת אינטרנט, או לתקשר עם מספר רב של API-ים הממשיכים להסתמך על XML על פני JSON לחליפין נתונים.

## איך לעשות:

Google Apps Script מספקת את ה-`XmlService` לעבודה עם נתוני XML. להלן אנו מדגימים איך לנתח מחרוזת XML, לשנות את תוכנה, ולייצר מחרוזת XML חדשה.

ניתוח מחרוזת XML:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // רושם: Hello
}
```

לשינוי ה-XML, ייתכן ותרצה להוסיף איבר ילד חדש:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // רושם את מחרוזת ה-XML החדשה עם איבר הילד שנוסף
}
```

ייצור מחרוזת XML מאפס:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // מוציא: <root><child>Hello World</child></root>
}
```

## עיון מעמיק

בהיסטוריה, XML (שפת הסימון להרחבה) היה התקן המוביל לחליפין נתונים לפני ש-JASON התפתח כחלופה קלה יותר. תחביר המפורט של XML ומודל הניתוח המחמיר שלו הספקו פורמט נתונים אמין, אם כי מסורבל. ב-Google Apps Script, ממשק ה-API של `XmlService` מכיל את היצירה, הניתוח והעיבוד של נתוני XML, בהכרה בחשיבותם המתמשכת במגוון מערכות ירושה וארגוניות, בשירותי אינטרנט SOAP ובקובצי הגדרה ליישומים.

למרות השכיחות של JSON בפיתוח האינטרנט המודרני בשל פשטותו והקלות בשימושו עם JavaScript, XML נותר רלוונטי בתחומים שבהם חשובה אימות תיעוד והיררכיות מובנות. עם זאת, עבור פרויקטים חדשים, במיוחד אלה המתמקדים ב-APIs של אינטרנט, JSON לרוב יהווה את הבחירה המעשית יותר בשל טיבו הקליל והאינטגרציה החלקה עם JavaScript.

הבנה של XML וטיפולו ב-Google Apps Script היא קריטית עבור מפתחים העובדים בסביבות שבהן נחוצה אינטגרציה עם מערכות ישנות או עם API-ים ארגוניים ספציפיים. עם זאת, כאשר מתחילים פרויקטים חדשים או כאשר גמישות היא מפתח, מומלץ להעריך את הצורך ב-XML לעומת חלופות כמו JSON.
