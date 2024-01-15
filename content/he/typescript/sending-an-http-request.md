---
title:                "שליחת בקשת http"
html_title:           "TypeScript: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

למה מישהו ירצה לשלוח בקשת HTTP? בעצם, בקשת HTTP היא דרך קלה ויעילה לשלוח ולקבל מידע משרת אינטרנט מסוים. 

## איך לעשות זאת

ראשית, נצטרך ליצור אובייקט של XMLHttpRequest מובנה של TypeScript. הקוד הבא מדגים כיצד ניתן ליצור ולשלוח בקשת HTTP GET באמצעות XMLHttpRequest:

```TypeScript
let request = new XMLHttpRequest();
request.open('GET', 'https://example.com/data');
request.send();
```

כעת נבדוק את התשובה המתקבלת מהשרת. ניתן להשתמש בפונקציה המובנית `onreadystatechange` על מנת לאתר מתי השאילתה הסתיימה ולבדוק את קוד התגובה שנשלח חזרה מהשרת. הקוד הבא מתאר את התהליך הנוכחי:

```TypeScript
request.onreadystatechange = function() {
  if (request.readyState === XMLHttpRequest.DONE) {
    if (request.status === 200) {
      console.log(request.response);
    }
  }
};
```

בקוד הנ"ל, אנו בודקים את נתוני התגובה, ואם קוד התגובה הוא 200 (מציין שהבקשה נמצאת במצב הצליחה), אנו מדפיסים את התוכן של השאילתה למסך.

## חקירה מעמיקה

אנו יכולים להשתמש גם בטכניקת "Promise" כדי לנהל את בקשת ה-HTTP בצורה אסינכרונית. פונקציית ה-`fetch` המובנית של TypeScript מחזירה Promise, שמאפשר לנו לנהל את תהליך יצירת הבקשה וקבלת התשובה בצורה מסודרת יותר. 

בכדי להשתמש ב-Promise, ניתן להשתמש במתודת ה-`then` או במתודת ה-`catch` על התוצאה המוחזרת מפונקציית ה-`fetch`. הקוד הבא מדגים כיצד ניתן לשלוח בקשת HTTP GET באמצעות טכניקת Promise:

```TypeScript
fetch('https://example.com/data')
  .then(response => response.json()) // ממיר נתוני התגובה לפורמט JSON
  .then(data => console.log(data)) // מדפיס נתוני התגובה למסך
  .catch(error => console.log(error)); // מטפל בשגיאות