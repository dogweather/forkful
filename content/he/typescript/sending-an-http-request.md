---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T18:00:57.448884-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/sending-an-http-request.md"
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
