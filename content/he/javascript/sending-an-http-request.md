---
title:                "שליחת בקשת http"
html_title:           "Javascript: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

למה מישהו יעסוק בשליחת בקשת HTTP? בעצם, בכדי לתקשר עם שרת חיצוני ולקבל תגובה ממנו. זה נחשב כפרוטוקול תקשורת רחב מקובל לאתרים ויישומים מרובי-שימוש ומאפשר למשתמש לבצע פעולות באתרים שונים כמו לעדכן מידע או להתחבר לחשבון משתמש.

## כיצד לשלוח בקשת HTTP ב-Javascript

תחילה, יש ליצור עצם XMLHttpRequest חדש שישמש כאמצעי תקשורת לשלוח את הבקשה. לאחר מכן, יש להגדיר את שיטת הפנייה, כתובת ה-URL וטיפוס הנתונים שנשלחים בגוף הבקשה. לבסוף, יש להגדיר פונקציה שתתבצע בעת סיום שליחת הבקשה כדי לקבל ולעבד את התגובה מהשרת.

```javascript
let httpRequest = new XMLHttpRequest();
httpRequest.open("POST", "https://www.example.com/api", true);
httpRequest.setRequestHeader('Content-Type', 'application/json');
httpRequest.onreadystatechange = function() {
  if (httpRequest.readyState === XMLHttpRequest.DONE) {
    // עיבוד התגובה מהשרת
  }
}
httpRequest.send(JSON.stringify(data));
```

### דוגמא ליצירת בקשת POST

נניח שאנו רוצים לשלוח בקשת POST עם גוף הבקשה המכיל מידע משתמש כמו שם ואימייל לשרת הרשמה. בכדי לעשות זאת, אנו משתמשים בפונקציה `JSON.stringify()` כדי להמיר את המידע לתבנית JSON ומשתמשים בטיפוס הנתונים המתאים כמו מחרוזת או מערך עבור כל אחד מהשדות של הטופס.

```javascript
let data = {
  name: "John Smith",
  email: "john.smith@example.com"
};

let httpRequest = new XMLHttpRequest();
httpRequest.open("POST", "https://www.example.com/register", true);
httpRequest.setRequestHeader('Content-Type', 'application/json');
httpRequest.onreadystatechange = function () {
  if (httpRequest.readyState === XMLHttpRequest.DONE) {
    let response = JSON.parse(httpRequest.responseText); // עיבוד תגובת השרת בתבנית JSON
    console.log(response.message); // "המשתמש נרשם בהצלחה"
  }
}
httpRequest.send(JSON.stringify(data));
```

## העמקת נתונים על שליחת בקשת HTTP

ה-XMLHttpRequest הוא רכיב חיוני בטכנולוגיית ה-A