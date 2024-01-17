---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Javascript: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא פעולה שמאפשרת למתכנתים לשלוח ולקבל מידע מאתרים ושירותים באמצעות פרוטוקול האינטרנט. האימות הבסיסי מבטיח שרק משתמשים מורשים יכולים לגשת למידע המסופק.

## כיצד לעשות זאת:
כתיבת בקשת HTTP עם אימות בסיסי מתבצעת באמצעות הוספת כותרת מתאימה לבקשה. הנה דוגמה להרחבה של כותרת זו בשפת ג'אווהסקריפט:
```javascript
const username = 'my_username';
const password = 'my_password';

// הפעלת בקשת HTTP עם אימות בסיסי
fetch('https://example.com/api', {
  headers: {
    Authorization: `Basic ${btoa(`${username}:${password}`)}`
  }
})
  .then(response => response.json())
  .then(data => console.log(data));
```

כאשר מישתמשים בשפת ג'אווהסקריפט, הם יכולים גם להשתמש בספריות כדוגמת Axios כדי לפשט עוד יותר את התהליך:
```javascript
const axios = require('axios');

const username = 'my_username';
const password = 'my_password';

// שליחת בקשת HTTP עם אימות בסיסי באמצעות Axios
axios.get('https://example.com/api', {
  auth: {
    username,
    password
  }
})
  .then(response => console.log(response.data))
```

## מעמקים:
אימות הבסיסי נוצר בשנות השמונים כפעולה נפוצה בפרוטוקול האינטרנט. היו דרכים קודמות לזה, אך רבותן לא היו כל כך בטוחות ונסמכו על תעודות TLS/SSL החדשות יותר.

בינתיים, ישנן תקן נוסף דרך השתמשות בהסכם צמיתות (Digest Authentication). התקן זה מבטיח אימות עוד יותר אבטחתי, אך הוא מצריך יותר חישובים ובניית הודעות מתוקנות יותר.

כאשר משתמשים באימות בסיסי, חשוב להשתמש בחיבור מאובטח (HTTPS) כדי למנוע ניגודיות כנגד אימות בסיסי במקומות לא מאובטחים.

## ראו גם:
למידע נוסף על אימות בסיסי ניתן להיפגש במקומות הבאים:

- [MDN פנקס המידע למתכנתים](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [ויקיפדיה באנגלית](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [מפרט התקן של האינטרנט לאימות בסיסי](https://tools.ietf.org/html/rfc7617)