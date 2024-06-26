---
date: 2024-01-26 04:33:09.215633-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: XML \u05D4\u05D5\u05D0\
  \ \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\u05EA \u05E9\u05DC eXtensible\
  \ Markup Language, \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05E9\u05E7\u05D9\u05D9\u05DD \u05DE\u05D0\u05D6 \u05E1\u05D5\u05E3 \u05E9\
  \u05E0\u05D5\u05EA \u05D4-90. \u05D4\u05D5\u05D0 \u05DE\u05D2\u05D3\u05D9\u05E8\
  \ \u05E1\u05D8 \u05DB\u05DC\u05DC\u05D9\u05DD \u05DC\u05E7\u05D9\u05D3\u05D5\u05D3\
  \ \u05DE\u05E1\u05DE\u05DB\u05D9\u05DD \u05E9\u05DF\u05DD \u05E7\u05E8\u05D9\u05D0\
  \u05D9\u05DD \u05D2\u05DD \u05DC\u05D0\u05D3\u05DD \u05D5\u05D2\u05DD\u2026"
lastmod: '2024-04-05T22:50:54.070303-06:00'
model: gpt-4-0125-preview
summary: "XML \u05D4\u05D5\u05D0 \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\
  \u05EA \u05E9\u05DC eXtensible Markup Language, \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05E7\u05D9\u05D9\u05DD \u05DE\u05D0\u05D6\
  \ \u05E1\u05D5\u05E3 \u05E9\u05E0\u05D5\u05EA \u05D4-90."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
weight: 40
---

## איך לעשות:
הנה איך לנתח XML:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>User</to>
                    <from>Author</from>
                    <heading>Reminder</heading>
                    <body>אל תשכח אותי הסופ"ש הזה!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// פלט: User
```

ולייצר XML:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'User';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// פלט: <note><to>User</to></note>
```

## צלילה עמוקה
XML הוא ראשי תיבות של eXtensible Markup Language, פורמט נתונים שקיים מאז סוף שנות ה-90. הוא מגדיר סט כללים לקידוד מסמכים שןם קריאים גם לאדם וגם למכונה. היסטורית, XML זכה לתשומת לב בשל הגמישות וההיררכיה המובנית שלו, מה שהפך אותו לבחירה פופולרית עבור שירותי רשת, כמו SOAP, ומספר רב של קבצי תצורה.

חלופות ל-XML כוללות את JSON (JavaScript Object Notation), שהפך לפופולרי בשל נוחיות השימוש שלו עם JavaScript והמשקל הקל יותר שלו. YAML הוא חלופה נוספת, המוערכת בשל היותה ידידותית לאדם ובחירה נפוצה לתצורה.

XML מיושם ב-JavaScript באמצעות ממשקי DOMParser ו-XMLSerializer. דום XML (Document Object Model) מאפשר לנווט ולערוך מסמכי XML בדיוק כמו שעושים עם HTML. למרות עלייתו של JSON, הבנה של XML חשובה, מכיוון שמערכות ישנות רבות ותעשיות מסוימות עדיין מסתמכות עליו להחלפת נתונים.

## ראו גם
- MDN Web Docs (ניתוח XML): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (מדריך XML DOM): https://www.w3schools.com/xml/dom_intro.asp
- "מה זה XML?": https://www.w3.org/XML/
