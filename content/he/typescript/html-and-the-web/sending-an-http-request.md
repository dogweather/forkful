---
aliases:
- /he/typescript/sending-an-http-request/
date: 2024-01-20 18:00:57.448884-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D6\u05D4\
  \ \u05D4\u05D3\u05E8\u05DA \u05DC\u05D3\u05D1\u05E8 \u05E2\u05DD \u05E9\u05E8\u05EA\
  \u05D9\u05DD \u05D1\u05E8\u05E9\u05EA. \u05E4\u05E8\u05D5\u05D2\u05E8\u05DE\u05E8\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\
  \u05D9 \u05DC\u05D4\u05D7\u05DC\u05D9\u05E3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  , \u05DC\u05D2\u05E9\u05EA \u05DC-APIs, \u05D5\u05DC\u05D0\u05E4\u05E9\u05E8 \u05DC\
  \u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D5\u05EA \u05E9\u05DC\u05D4\u05DD\
  \ \u05DC\u05EA\u05E7\u05E9\u05E8."
lastmod: 2024-02-18 23:08:52.566920
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D6\u05D4\
  \ \u05D4\u05D3\u05E8\u05DA \u05DC\u05D3\u05D1\u05E8 \u05E2\u05DD \u05E9\u05E8\u05EA\
  \u05D9\u05DD \u05D1\u05E8\u05E9\u05EA. \u05E4\u05E8\u05D5\u05D2\u05E8\u05DE\u05E8\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\
  \u05D9 \u05DC\u05D4\u05D7\u05DC\u05D9\u05E3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  , \u05DC\u05D2\u05E9\u05EA \u05DC-APIs, \u05D5\u05DC\u05D0\u05E4\u05E9\u05E8 \u05DC\
  \u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D5\u05EA \u05E9\u05DC\u05D4\u05DD\
  \ \u05DC\u05EA\u05E7\u05E9\u05E8."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
שליחת בקשת HTTP זה הדרך לדבר עם שרתים ברשת. פרוגרמרים עושים את זה כדי להחליף נתונים, לגשת ל-APIs, ולאפשר לאפליקציות שלהם לתקשר.

## How to (איך לעשות את זה):
הנה קטע קוד TypeScript ששולח בקשת GET בסיסית עם `fetch` ומחזיר את התוצאה:

```TypeScript
async function fetchTodos() {
  try {
    const response = await fetch('https://jsonplaceholder.typicode.com/todos/1');
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error('Error fetching data', error);
  }
}

fetchTodos();
```

תוצאת דוגמה:

```json
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

## Deep Dive (צלילה עמוקה)
שליחת בקשות HTTP החלה בשנות ה-90 עם תחילת ה-Web. כיום, קיימים מספר אופציות ב-TypeScript, כמו `XMLHttpRequest`, `fetch`, וספריות כמו Axios. `fetch` היא מודרנית ומבוססת Promises, ולכן נוחה לשימוש. עליך לטפל בשגיאות נכון ולשים לב למידע שאתה שולח ומקבל.

## See Also (ראו גם)
- [MDN Web Docs - fetch()](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- [Axios library](https://github.com/axios/axios)
- [JSONPlaceholder for fake Online REST API](https://jsonplaceholder.typicode.com/)
