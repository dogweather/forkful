---
date: 2024-01-20 18:00:57.448884-07:00
description: "How to (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D0\u05EA\
  \ \u05D6\u05D4): \u05D4\u05E0\u05D4 \u05E7\u05D8\u05E2 \u05E7\u05D5\u05D3 TypeScript\
  \ \u05E9\u05E9\u05D5\u05DC\u05D7 \u05D1\u05E7\u05E9\u05EA GET \u05D1\u05E1\u05D9\
  \u05E1\u05D9\u05EA \u05E2\u05DD `fetch` \u05D5\u05DE\u05D7\u05D6\u05D9\u05E8 \u05D0\
  \u05EA \u05D4\u05EA\u05D5\u05E6\u05D0\u05D4."
lastmod: '2024-03-13T22:44:38.912168-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E0\u05D4 \u05E7\u05D8\u05E2 \u05E7\u05D5\u05D3 TypeScript \u05E9\
  \u05E9\u05D5\u05DC\u05D7 \u05D1\u05E7\u05E9\u05EA GET \u05D1\u05E1\u05D9\u05E1\u05D9\
  \u05EA \u05E2\u05DD `fetch` \u05D5\u05DE\u05D7\u05D6\u05D9\u05E8 \u05D0\u05EA \u05D4\
  \u05EA\u05D5\u05E6\u05D0\u05D4."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
weight: 44
---

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
