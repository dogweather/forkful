---
title:                "הורדת דף אינטרנט"
aliases:
- /he/typescript/downloading-a-web-page/
date:                  2024-01-20T17:45:28.226024-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה להוריד דף אינטרנט, ולמה זה נחוץ? הורדת דף אינטרנט היא פעולה שבה אנחנו משיגים את תוכן האינטרנט משרת למחשב או לאפליקציה שלנו. תכניתנים עושים את זה כדי לעבד נתונים, לאסוף מידע או לאחסן גיבוי של התוכן.

## How to:
```TypeScript
import axios from 'axios';

(async () => {
  try {
    const response = await axios.get('https://www.example.com');
    console.log(response.data);
  } catch (error) {
    console.error('Error fetching page: ', error);
  }
})();
```
תוצאה (Sample output):
```
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title>
  ...
</head>
<body>
  <h1>Example Domain</h1>
  ...
</body>
</html>
```

## Deep Dive
היסטוריה - פעם, נדרשו תוכניות בשורת הפקודה כמו `curl` בלינוקס או `Invoke-WebRequest` ב-Windows PowerShell להורדת דפי אינטרנט. כיום, קיימות ספריות כמו `axios` שמקלות על המשימה.

אלטרנטיבות - מעבר ל־`axios`, ניתן להשתמש גם ב־`fetch-API` שנתמך ב־Node.js או ספריות כמו `request-promise` (שים לב ש`request` אינה נתמכת יותר).

פרטי יישום - כאשר מורידים דף אינטרנט, חשוב לטפל ב־HTTP errors ו timeouts. בדוגמה שלמעלה, השימוש ב־try/catch מאפשר לכידת וטיפול בשגיאות באופן נאות.

## See Also
- [Axios GitHub repository](https://github.com/axios/axios)
- [MDN Web Docs - Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Node.js Documentation - HTTP GET requests](https://nodejs.org/api/http.html#http_http_get_options_callback)
