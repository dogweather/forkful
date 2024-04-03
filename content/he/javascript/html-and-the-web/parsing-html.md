---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:02:03.154606-07:00
description: "\u05E0\u05D9\u05EA\u05D5\u05D7 HTML \u05DE\u05E9\u05DE\u05E2\u05D5 \u05E9\
  \u05DC\u05D9\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05DE\u05E1\
  \u05DE\u05DB\u05D9 HTML. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05D4\
  \u05EA\u05E2\u05E8\u05D1 \u05D0\u05D5 \u05DC\u05E9\u05E0\u05D5\u05EA \u05EA\u05D5\
  \u05DB\u05DF \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8\u05D9, \u05DC\u05D0\u05D5\
  \u05D8\u05DE\u05D8 \u05D0\u05EA \u05E9\u05DC\u05D9\u05E4\u05EA \u05D4\u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD, \u05D0\u05D5 \u05DC\u05E6\u05D5\u05E8\u05DB\u05D9 \u05D2\
  \u05E8\u05D9\u05E4\u05EA \u05E8\u05E9\u05EA."
lastmod: '2024-03-13T22:44:39.970989-06:00'
model: gpt-4-0125-preview
summary: "\u05E0\u05D9\u05EA\u05D5\u05D7 HTML \u05DE\u05E9\u05DE\u05E2\u05D5 \u05E9\
  \u05DC\u05D9\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05DE\u05E1\
  \u05DE\u05DB\u05D9 HTML."
title: "\u05E4\u05D9\u05E8\u05D5\u05E7 HTML"
weight: 43
---

## מה ולמה?
ניתוח HTML משמעו שליפת נתונים ממסמכי HTML. מתכנתים עושים זאת על מנת להתערב או לשנות תוכן אינטרנטי, לאוטמט את שליפת הנתונים, או לצורכי גריפת רשת.

## איך לעשות:
בואו ננתח HTML באמצעות ה-API `DOMParser` ב-JavaScript.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>שלום, עולם!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // פלט: שלום, עולם!
```

עכשיו, בואו נתפוס משהו יותר ספציפי, כמו אלמנט עם מחלקה:

```Javascript
const htmlString = `<div><p class="greeting">שלום, שוב!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // פלט: שלום, שוב!
```

## צלילה עמוקה
ניתוח HTML הוא כבר ישן כמו הרשת עצמה. בתחילה, זה היה דבר של דפדפנים—דפדפנים נתחו HTML כדי להציג דפי אינטרנט. עם הזמן, מתכנתים רצו לחדור לתהליך הזה, מה שהוביל ל-APIs כמו `DOMParser`.

אלטרנטיבות? בטח. יש לנו ספריות כמו `jQuery` וכלים כמו `BeautifulSoup` ל-Python. אך ה-`DOMParser` המקורי של JavaScript מהיר ומובנה, אין צורך בספריות נוספות.

מבחינת היישום, כאשר אתה מנתח HTML עם `DOMParser`, הוא יוצר אובייקט `Document`. חשוב על זה כמודל היררכי של ה-HTML שלך. כשיש לך את זה, אתה יכול לנווט ולשנות אותו בדיוק כמו שהיית עושה עם DOM של דף אינטרנט רגיל.

הנה העניין—ניתוח יכול להיתקל בבעיות עם HTML לקוי. דפדפנים סולחים, אבל `DOMParser` עשוי שלא להיות. לכן, למשימות מורכבות או HTML מבולגן, ספריות צד שלישי עשויות לעשות עבודה נקייה יותר.

## ראו גם
- מסמכי האינטרנט של MDN על ה-API `DOMParser`: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- יכולות הניתוח של jQuery: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, מימוש מהיר, גמיש ורזה של ליבת jQuery עבור השרת: [Cheerio.js](https://cheerio.js.org/)
- לניתוח לא ב-JS: ספריית BeautifulSoup של Python: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
