---
title:                "בדיקת קיומו של תקיה"
html_title:           "Javascript: בדיקת קיומו של תקיה"
simple_title:         "בדיקת קיומו של תקיה"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Why: 
נבחרת הקוד היא היכולת לוודא אם תיקייה קיימת במחשב. זה יכול להיות שימושי במצבים שבהם נדרש לבצע פעולות שונות בתיקיות שונות וכדי למנוע בעיות כאשר התיקייה אינה קיימת.

How To: 
```Javascript 
// קריאת הפקודה "fs" שמאפשרת לנו לבצע פעולות בקבצים ותיקיות
const fs = require('fs');

// בדיקה אם התיקייה "test" קיימת במחשב
fs.existsSync('./test');

// פלט: אמת או שקר בהתאם לתוצאה
```

Deep Dive: 
הפונקציה "existsSync" במודול "fs" מאפשרת לנו לוודא באופן סינכרוני אם התיקייה קיימת במחשב. אם התיקייה קיימת, הפונקציה תחזיר אמת ואם היא אינה קיימת תחזיר שקר. אם אנו רוצים לבצע בדיקה אסינכרונית, ניתן להשתמש בפונקציה "exists" שמחזירה מבטיחה עם התיקייה קיימת או לא.

See Also:
תנאי if-else ב-Javascript: https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Statements/if...else
המודול "fs" בחבילת Node.js: https://nodejs.org/api/fs.html