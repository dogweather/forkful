---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T18:00:45.089885-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/sending-an-http-request.md"
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