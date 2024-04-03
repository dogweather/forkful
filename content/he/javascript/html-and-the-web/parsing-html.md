---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:02:03.154606-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05D5\u05D0\
  \u05D5 \u05E0\u05E0\u05EA\u05D7 HTML \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\
  \ \u05D4-API `DOMParser` \u05D1-JavaScript."
lastmod: '2024-03-13T22:44:39.970989-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D5\u05D0\u05D5 \u05E0\u05E0\u05EA\u05D7 HTML \u05D1\u05D0\u05DE\
  \u05E6\u05E2\u05D5\u05EA \u05D4-API `DOMParser` \u05D1-JavaScript."
title: "\u05E4\u05D9\u05E8\u05D5\u05E7 HTML"
weight: 43
---

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
