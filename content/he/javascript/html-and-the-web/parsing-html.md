---
title:                "פירוק HTML"
aliases: - /he/javascript/parsing-html.md
date:                  2024-01-28T03:02:03.154606-07:00
model:                 gpt-4-0125-preview
simple_title:         "פירוק HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/parsing-html.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
