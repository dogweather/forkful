---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה זה אומר ולמה? 

שליחת בקשת HTTP עם אוטנטיקציה בסיסית מאפשרת לשלוח בקשות WEB מהמסוף שלך לשרת שדורש ממך שם משתמש וסיסמה. תכנתים משתמשים בזה כדי לגשת למשאבים מאובטחים שמחייבים אימות.

## איך לעשות:

לדוגמה, הפקודה הבאה משתמשת ב-curl כדי לשלוח בקשת GET עם אוטנטיקציה בסיסית:
```Bash
curl -u username:password http://localhost:8080/resource
```
אותו משימה באמצעות wget תיראה כך:
```Bash
wget --user username --password 'password' http://localhost:8080/resource
```
אתה צריך להחליף username:password עם שם המשתמש והסיסמה שלך. 

## צלילה עמוקה

בראשית היישום של HTTP אין אופציה לאימות. בהמשך, אימות בסיסי הוזן לפרוטוקול. הרעיון מאחורי זה הוא למנוע ממשתמשים שאינם מורשים לגשת למשאב. למרות שאימות בסיסי ב-HTTP נמשך, הוא מוגבל במידת ההצפנה שלו.

חלופות לאימות בסיסי ב-HTTP כוללות אימות Digest, אימות NTLM, ואימות אסימטרי.

## ראה גם

1. Curl User Guide, http://curl.haxx.se/docs/manual.html
2. Wget User Guide, https://www.gnu.org/software/wget/manual/html_node/index.html
3. HTTP Authentication, https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication