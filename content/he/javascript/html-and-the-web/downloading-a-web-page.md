---
title:                "הורדת דף אינטרנט"
aliases: - /he/javascript/downloading-a-web-page.md
date:                  2024-01-20T17:44:20.114688-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

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
