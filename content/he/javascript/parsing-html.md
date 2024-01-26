---
title:                "ניתוח HTML"
date:                  2024-01-20T15:32:47.684437-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פיענוח HTML (Parsing) הוא קריאה ואנליזה של קוד HTML כדי למנף את התוכן והמבנה שלו בתוך תוכנית JavaScript. מתכנתים עושים את זה כדי לטעון נתונים דינמיים, למשל מ-API, לשנות או לאנליז את המסמך, ולעבוד עם מידע בדפי אינטרנט.

## איך לעשות:
כדי לפענח HTML ב-JavaScript, ניתן להשתמש במחלקה `DOMParser`. דוגמא:

```javascript
const htmlString = '<div>שלום עולם!</div>';
const parser = new DOMParser();
const doc = parser.parseFromString(htmlString, 'text/html');

console.log(doc.body.textContent); // יודפס: שלום עולם!
```

להלן דוגמא נוספת, למציאת קישורים בתוך מחרוזת HTML:

```javascript
const htmlString = '<a href="https://example.com">לדוגמא</a>';
const doc = parser.parseFromString(htmlString, 'text/html');
const links = doc.querySelectorAll('a');

links.forEach(link => console.log(link.href)); // יודפס: https://example.com
```

## עיון נוסף:
כלי הפיענוח HTML, `DOMParser`, הודגש בתחילה בספציפיקציות DOM Level 2. זה מאפשר פיתרון פשוט ויעיל לטעינת קוד HTML לאובייקט DOM שיהיה נגיש ב-JavaScript. לאלו שעובדים עם Node.js, מודולים כמו `node-html-parser` חלופה נפוצה, כיון ש`DOMParser` אינו זמין כברירת מחדל בסביבת Node. בנוסף, חשוב לשים לב שפיענוח HTML יכול להיחשב פעולה מתמשכת ולכן יש להשתמש בו בזהירות בקוד שמופעל בדפדפן, כדי למנוע קפיצות בביצועים או איטיות.

## ראו גם:
- [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [`node-html-parser` on npm](https://www.npmjs.com/package/node-html-parser) - מודול לפיענוח HTML ב-Node.js.
- [HTML Living Standard](https://html.spec.whatwg.org/) - המפרט הרשמי ל-HTML מכיל פרטים על איך דפדפנים צריכים להתייחס למבנה קוד HTML.
