---
title:                "קריאת קובץ טקסט"
html_title:           "Javascript: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מדוע

מהו המטרה שבעלי פנים תחום מחשבים ירצו לקרוא קובץ טקסט? קריאת קובץ טקסט היא כלי חשוב בכתיבת קוד ב-Javascript ועשויה להיות נחוצה למגוון רב של פעולות ופרויקטים. במאמר זה תלמדו איך לקרוא קובץ טקסט באמצעות קוד Javascript ותקבלו הנחיות טכניות שיעזרו לכם להיות מוצלחים בכתיבת קוד.

## איך לעשות זאת

כדי לקרוא קובץ טקסט ב-Javascript, אנו צריכים להשתמש במודול מובנה בשם "fs" (שמורת קבצים). באמצעות הפונקציה "readFile" במודול זה, אנו יכולים לקרוא ולטעון את התוכן של קובץ טקסט לתוך משתנה. הנה דוגמא קודם לכל:

```Javascript 
// require fs module 
let fs = require('fs'); 

// read file 'example.txt'
fs.readFile('example.txt', (err, data) => { 
    if (err) throw err; 

    // print file content 
    console.log(data.toString()); 
}); 
```
התוכן של הקובץ יתועד במשתנה "data" ובעזרת הפונקציה "toString" אנו יכולים להדפיס אותו למסך. יש לזכר שכאשר אנו משתמשים בפונקציה "readFile" התוכן של הקובץ ייטען כולו לפני שהפעולה הבאה בקוד יחושבה.

## חפר עמוק

כבר ידענו איך לקרוא קובץ טקסט ולהדפיס את התוכן שלו, אבל יש עוד שימושים נפוצים בקריאת קבצים טקסט ב-Javascript. כאשר אנו עובדים עם קבצים גדולים או עם תוכן מבולגן, ייתכן שנרצה לחלק את התוכן לחלקים קטנים יותר. לדוגמה, אם אנו רוצים לטעון רק את התוכן של השורה הראשונה בקובץ, נוכל להשתמש ב