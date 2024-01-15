---
title:                "הורדת עמוד אינטרנט"
html_title:           "Javascript: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

מדוע: כי הורדת דף אינטרנט נחשבת לתחום חשוב ונפוץ בתחום התכנות הווב. זה נחשב לכלי חיוני בבניית אתרים מתקדמים ומאפשר למפתחים ליצור חוויית משתמש מתוחכמת ועשירה.

איך לעשות: בואו נתחיל ללמוד כיצד להוריד דף אינטרנט באמצעות קוד Javascript. עקובו אחר הקודים המוצגים תחת הבלוקים הללו וראו איך הוא עובד ומה הפלט שלו.

```Javascript
// החדשנות הראשונה בג'אווהסקריפט
var request = new XMLHttpRequest();

// מגדירים את הכתובת של הדף שנרצה להוריד
var url = "https://www.example.com";

// פתחו את הבקשה ותפריט המצבים ל 200 לכל הדפים המקוונים
request.open('GET', url);

// מפעילים את הפונקציה שתכשיל לקחת את הפלט בזמן הורדת הדף
request.onload = function() {

// בודקים האם קיבלנו קוד מצב 200, שמרצה על הצלחה
if (request.status === 200) {

// מדפיס לקונסולה את התוצאה שקיבלנו
console.log(request.responseText);
}
}

// שולחים את הבקשה
request.send();
```

מצאו דוגמאות נוספות והסברים מפורטים יותר על כיצד להוריד דפים באתרים רלוונטיים וכיצד לעבוד עם תגובות וקבצים מרובים באמצעות ג'אווסקריפט במדריך הזה: [כיצד להוריד דף אינטרנט עם ג'אווסקריפט](https://www.w3schools.com/js/js_window_location.asp)

עומק השקעה: להוריד דף אינטרנט באמצעות קוד Javascript נותן לנו את האפשרות ליצור אפליקציות אינטרנט מתקדמות יותר ולשלב תכונות ופיצ'רים מגוונים על הדף. בכך ניתן ליצור חווית משתמש מתוחכמ