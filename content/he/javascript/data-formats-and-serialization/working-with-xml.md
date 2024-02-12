---
title:                "עבודה עם XML"
aliases:
- /he/javascript/working-with-xml.md
date:                  2024-01-26T04:33:09.215633-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML פירושה לנתח, לעבד ולייצר תוכן XML באמצעות קוד. תכנתים עושים זאת מכיוון ש-XML משמש רחבות לקבצי תצורה, החלפת נתונים, ושירותי רשת, בשל טיבו הקריא לאדם והניתן לניתוח על ידי מכונה.

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
