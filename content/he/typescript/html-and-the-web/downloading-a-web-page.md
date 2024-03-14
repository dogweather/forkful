---
date: 2024-01-20 17:45:28.226024-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05D3\
  \u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8, \u05D5\u05DC\u05DE\u05D4 \u05D6\
  \u05D4 \u05E0\u05D7\u05D5\u05E5? \u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\
  \u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05E9\u05D9\u05D2\
  \u05D9\u05DD \u05D0\u05EA \u05EA\u05D5\u05DB\u05DF \u05D4\u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8 \u05DE\u05E9\u05E8\u05EA \u05DC\u05DE\u05D7\u05E9\u05D1 \u05D0\
  \u05D5 \u05DC\u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D4 \u05E9\u05DC\u05E0\
  \u05D5. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9\u2026"
lastmod: '2024-03-13T22:44:38.915229-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05D3\u05E3\
  \ \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8, \u05D5\u05DC\u05DE\u05D4 \u05D6\u05D4\
  \ \u05E0\u05D7\u05D5\u05E5? \u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\
  \u05E0\u05D8\u05E8\u05E0\u05D8 \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4\
  \ \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05E9\u05D9\u05D2\u05D9\
  \u05DD \u05D0\u05EA \u05EA\u05D5\u05DB\u05DF \u05D4\u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8 \u05DE\u05E9\u05E8\u05EA \u05DC\u05DE\u05D7\u05E9\u05D1 \u05D0\u05D5\
  \ \u05DC\u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D4 \u05E9\u05DC\u05E0\u05D5\
  . \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9\u2026"
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
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
