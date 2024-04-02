---
date: 2024-01-20 17:44:20.114688-07:00
description: "\u05DB\u05D0\u05E9\u05E8 \u05D0\u05E0\u05D7\u05E0\u05D5 '\u05DE\u05D5\
  \u05E8\u05D9\u05D3\u05D9\u05DD' (downloading) \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8, \u05D0\u05E0\u05D5 \u05E9\u05D5\u05DC\u05E4\u05D9\u05DD \u05EA\
  \u05D5\u05DB\u05DF \u05DE\u05D0\u05EA\u05E8 \u05DB\u05DC\u05E9\u05D4\u05D5 \u05D5\
  \u05E9\u05D5\u05DE\u05E8\u05D9\u05DD \u05D0\u05D5\u05EA\u05D5 \u05DC\u05E9\u05D9\
  \u05DE\u05D5\u05E9 \u05DE\u05E7\u05D5\u05DE\u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05E2\u05D1\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05EA\u05E4\
  \u05D5\u05E1 \u05DE\u05D9\u05D3\u05E2 \u05D7\u05D9\u05D5\u05E0\u05D9,\u2026"
lastmod: '2024-03-13T22:44:39.972637-06:00'
model: gpt-4-1106-preview
summary: "\u05DB\u05D0\u05E9\u05E8 \u05D0\u05E0\u05D7\u05E0\u05D5 '\u05DE\u05D5\u05E8\
  \u05D9\u05D3\u05D9\u05DD' (downloading) \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8, \u05D0\u05E0\u05D5 \u05E9\u05D5\u05DC\u05E4\u05D9\u05DD \u05EA\u05D5\
  \u05DB\u05DF \u05DE\u05D0\u05EA\u05E8 \u05DB\u05DC\u05E9\u05D4\u05D5 \u05D5\u05E9\
  \u05D5\u05DE\u05E8\u05D9\u05DD \u05D0\u05D5\u05EA\u05D5 \u05DC\u05E9\u05D9\u05DE\
  \u05D5\u05E9 \u05DE\u05E7\u05D5\u05DE\u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05E2\u05D1\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05EA\u05E4\u05D5\
  \u05E1 \u05DE\u05D9\u05D3\u05E2 \u05D7\u05D9\u05D5\u05E0\u05D9,\u2026"
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
weight: 42
---

## מה ולמה?
כאשר אנחנו 'מורידים' (downloading) דף אינטרנט, אנו שולפים תוכן מאתר כלשהו ושומרים אותו לשימוש מקומי. מתכנתים עושים זאת כדי לעבד נתונים, לתפוס מידע חיוני, או ליצור גיבוי לנתונים.

## איך לעשות:
קוד ב-JavaScript להורדת דף אינטרנט:

```javascript
const https = require('https');
const fs = require('fs');

const downloadPage = (url, filename) => {
  https.get(url, (response) => {
    let data = '';
    response.on('data', (chunk) => {
      data += chunk;
    });
    response.on('end', () => {
      fs.writeFile(filename, data, (error) => {
        if (error) {
          console.error('Error writing to file:', error);
        } else {
          console.log('Page downloaded to', filename);
        }
      });
    });
  }).on('error', (error) => {
    console.error('Error downloading page:', error);
  });
};

downloadPage('https://example.com', 'local_page.html');
```

פלט (Sample Output):

```
Page downloaded to local_page.html
```

## עיון בעומק
הורדת דף אינטרנט היא חלק מהשגת נתונים ברשת, מושג שקיים מאז שהאינטרנט פוצל לשימוש רחב. בזמן ש-HTTP GET הוא השיטה הנפוצה לבצע זאת, ישנן שיטות נוספות כגון אמצעי של API או אף מנועי חיפוש מותאמים אישית. במקרה שלנו, שימוש במודול ה-'https' של Node.js הוא דרך יעילה וישירה. חשוב לזכור שהורדת תכנים צריכה להתבצע בהתאם למדיניות השימוש ותקנון האתר ממנו מורידים.

## למידע נוסף:
- [Node.js Documentation for the HTTPS module](https://nodejs.org/api/https.html)
- [MDN Web Docs on the Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
