---
date: 2024-01-20 17:45:28.226024-07:00
description: "How to: \u05D4\u05D9\u05E1\u05D8\u05D5\u05E8\u05D9\u05D4 - \u05E4\u05E2\
  \u05DD, \u05E0\u05D3\u05E8\u05E9\u05D5 \u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA\
  \ \u05D1\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4 \u05DB\u05DE\
  \u05D5 `curl` \u05D1\u05DC\u05D9\u05E0\u05D5\u05E7\u05E1 \u05D0\u05D5 `Invoke-WebRequest`\
  \ \u05D1-Windows PowerShell \u05DC\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E4\u05D9\
  \ \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05DB\u05D9\u05D5\u05DD, \u05E7\u05D9\
  \u05D9\u05DE\u05D5\u05EA \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA\u2026"
lastmod: '2024-04-05T22:50:53.160150-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05D9\u05E1\u05D8\u05D5\u05E8\u05D9\u05D4 - \u05E4\u05E2\u05DD, \u05E0\
  \u05D3\u05E8\u05E9\u05D5 \u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA \u05D1\u05E9\
  \u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4 \u05DB\u05DE\u05D5 `curl`\
  \ \u05D1\u05DC\u05D9\u05E0\u05D5\u05E7\u05E1 \u05D0\u05D5 `Invoke-WebRequest` \u05D1\
  -Windows PowerShell \u05DC\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E4\u05D9 \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05E0\u05D8."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
weight: 42
---

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
