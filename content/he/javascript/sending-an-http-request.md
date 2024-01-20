---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP היא התרחיש בו מנגנון או יישום יוצרים בקשה אל שרת בשרת עבור מרכיב של מידע, ערך או תוכנה. מתכנתים שולחים בקשות HTTP כדי ליצור דיאלוג דינמי עם יישום דינמי שנמצא על שרת.

## איך לבצע:

הנה דוגמה לשליחת בקשת HTTP באמצעות Javascript, באמצעות אובייקט XMLHttpRequest:

```Javascript 
var xhr = new XMLHttpRequest();
xhr.open('GET', 'https://api.example.com/data', true);
xhr.onreadystatechange = function () {
  if (xhr.readyState == 4 && xhr.status == 200)
    console.log(JSON.parse(xhr.responseText));
}
xhr.send();
```

בדוגמה זו, אנו שולחים בקשה "GET" אל 'https://api.example.com/data'. 

אם הבקשה מתבצעת בהצלחה (כלומר, אם הקוד החזרה ב-200 והבקשה הושלמה), אנו מחזירים את התגובה כאובייקט JSON.

## שיעור עמוק:

מאז שוליים המאה ה-20, בקשות HTTP היו חלק מהאינטרנט. ההיסטוריה הארוכה שלהן הביאה למספר אלטרנטיבות (כמו fetch API, jQuery AJAX, Axios) שמציעות לעתים קרובות קוד מרוכז יותר.

כאשר אנו שולחים בקשת HTTP, אנו מתארים את סוג הבקשה (POST, GET, PUT, DELETE, וכו'), את הכתובת (URL), ונתונים נוספים.

זכור שבקשות HTTP הן לא מוגנות מבחינת הפרטיות, ותמיד תלך למשתמש דרך שרת, אז לא היידה לשם מידע שאתה לא רוצה שנמסר.

## ראו גם:

- [HTTP | MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [XMLHttpRequest | MDN](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [Fetch API | MDN](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)