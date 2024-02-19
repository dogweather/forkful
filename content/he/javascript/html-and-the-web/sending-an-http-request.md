---
aliases:
- /he/javascript/sending-an-http-request/
date: 2024-01-20 18:00:45.089885-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D4\u05D9\
  \u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D4\u05D3\u05E4\u05D3\
  \u05E4\u05DF \u05D0\u05D5 \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05E0\u05D5 \u05DE\
  \u05D1\u05E7\u05E9 \u05DE\u05D9\u05D3\u05E2 \u05DE\u05E9\u05E8\u05EA \u05D0\u05D9\
  \u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\
  \u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DB\u05D3\
  \u05D9 \u05DC\u05E7\u05D1\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05E9\
  \u05DC\u05D5\u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D5\u05DC\u05EA\u05E7\
  \u05E9\u05E8 \u05E2\u05DD \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9\u05DD \u05DE\u05E8\
  \u05D5\u05D7\u05E7\u05D9\u05DD."
lastmod: 2024-02-18 23:08:53.244652
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D4\u05D9\
  \u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D4\u05D3\u05E4\u05D3\
  \u05E4\u05DF \u05D0\u05D5 \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05E0\u05D5 \u05DE\
  \u05D1\u05E7\u05E9 \u05DE\u05D9\u05D3\u05E2 \u05DE\u05E9\u05E8\u05EA \u05D0\u05D9\
  \u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\
  \u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DB\u05D3\
  \u05D9 \u05DC\u05E7\u05D1\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05E9\
  \u05DC\u05D5\u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D5\u05DC\u05EA\u05E7\
  \u05E9\u05E8 \u05E2\u05DD \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9\u05DD \u05DE\u05E8\
  \u05D5\u05D7\u05E7\u05D9\u05DD."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא פעולה שבה הדפדפן או הקוד שלנו מבקש מידע משרת אינטרנט. תוכניתנים משתמשים בזה כדי לקבל נתונים, לשלוח נתונים, ולתקשר עם שירותים מרוחקים.

## איך לעשות:
```Javascript
// שימוש ב-Fetch API לשליחת בקשת GET
fetch('https://jsonplaceholder.typicode.com/posts/1')
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(err => console.error('שגיאה במהלך שליחת הבקשה:', err));

// דוגמת תוצאה
{
  userId: 1,
  id: 1,
  title: "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  body: "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
}
```

```Javascript
// שימוש ב-Fetch API לשליחת בקשת POST
fetch('https://jsonplaceholder.typicode.com/posts', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({
    title: 'foo',
    body: 'bar',
    userId: 1,
  }),
})
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(err => console.error('שגיאה במהלך שליחת הבקשה:', err));

// דוגמת תוצאה
{
  title: 'foo',
  body: 'bar',
  userId: 1,
  id: 101
}
```

## עיון נוסף:
פעם, כדי לשלוח בקשת HTTP היינו משתמשים ב-XMLHttpRequest ובקוד פחות אינטואיטיבי. היום, ה-Fetch API מספק דרך עכשווית ומובנת יותר. ישנם גם כלים נוספים כמו Axios ו-jQuery.ajax, אבל Fetch נמצא כיום בכולם ולרוב אין צורך בדברים מורכבים יותר. כשמדובר בפרטים כמו תיקול שגיאות, טיפול בבקשות אסינכרוניות ושליטה על הבקשה, ה-Fetch API יכול להיות מקור לכאב ראש, אבל לא חייב. זכרו, תפקידנו להבין את הכלים שאנחנו משתמשים בהם ולדעת לנצל אותם נכון.

## ראו גם:
- [MDN Web Docs - העמוד הרשמי של Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [JSONPlaceholder - שירות לדוגמאות נתונים לבקשות](https://jsonplaceholder.typicode.com/)
- [Axios GitHub repository - למעוניינים בפתרון חלופי](https://github.com/axios/axios)
