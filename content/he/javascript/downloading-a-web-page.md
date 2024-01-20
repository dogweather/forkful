---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?

הורדת דף באינטרנט היא תהליך של שליפת קובץ HTML של דף אינטרנט. מתכנתים עושים את זה בדרך כלל כדי לנתח נתונים מהדף או לשמור עותק מקומי.

## איך לעשות:

```JavaScript
// דוגמא ב-Javascript 
const https = require('https');
const fs = require('fs');
const url = 'https://www.example.com';

https.get(url, res => {
  res.pipe(fs.createWriteStream('example.html'));
});
```
זה ישמור את הדף המתואר ב- 'https://www.example.com' לקובץ 'example.html' בספרייה הנוכחית.

## צלילה עמוקה:

### היסטוריה: 
הורדת דפי אינטרנט הייתה חלק מתכנות המחשבים מאז ימי האינטרנט הראשונים.

### אלטרנטיבות: 
ישנן ספריות JavaScript אחרות, כמו `axios` ו-`request`, שיכולות להוריד דפים מהאינטרנט.

### הרחבה: 
ההבנה של מה שקורה מאחורי הקלעים (כמו שליחת בקשות HTTP וקבלת תגובות) חשובה כדי למנוע בעיות ולשמן את הריצה של הקוד.

## ראה גם:

- [Node.js הסברים](https://nodejs.org/api/https.html)
- [MDN Web Docs - HTTP](https://developer.mozilla.org/he/docs/Web/HTTP)
- [השוואת ביבליות Node.js HTTP Request](https://www.npmtrends.com/axios-vs-download-vs-request-vs-superagent)