---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Bash: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

למה? בסך הכל אדם ישתמש בשאילתות HTTP עם אימות בסיסי על מנת לקבל גישה למידע שמוגש מאתר או שרת מסוים.

## How To

כך נמש: בשביל לשלוח שאילתת HTTP עם אימות בסיסי בשפת Bash אנו משתמשים בפקודת "curl". הנה דוגמא פשוטה:

```Bash
curl -u username:password https://example.com/api/data
```

כאן, אנו משתמשים בפרמטר "-u" להעברת שם משתמש וסיסמה כחלק מהכתובת URL. הפקודה תשלח בקשת HTTP GET עם הפרמטרים המתאימים המדויקים, ותחזיר תוכן מאתר האינטרנט עם אימות בסיסי.

לנוחיותכם, אנו יכולים להגדיר את שם המשתמש והסיסמה כפעולתיים במשתנה ולהשתמש בחלק מהפקודה:

```Bash
username="johndoe"
password="secret"

curl -u $username:$password https://example.com/api/data
```

בפלט, תראו את קוד התגובה של הבקשה הצלחת ואת התוכן המשובץ מאתר ה-URL.

## Deep Dive

בפקודת "curl", הפרמטר "-u" משמש להעברת שם משתמש וסיסמה, כמו גם להשתמש בפרוטוקול אוטומטי ובפרמטרי אימות מתאימים. מעבר לכך, ניתן להשתמש בפקודת "curl" ליצירת בקשות HTTP עם אימות בסיסי ופרוטוקולים אחרים ע"י קישור לקוד עמוד מותאם אישית.

ללמוד עוד על איך לשלוח שאילתות HTTP עם אימות בסיסי באמצעות פקודת "curl", ניתן להיכנס לדף המדריך הרשמי של הפקודה בכתובת האתר הבאה:

https://curl.haxx.se/docs/manpage.html 

## See Also

ראו גם:

- [פקודת curl ב- Bash](https://linuxize.com/post/curl-command-in-linux/) 
- [כיצד לבצע אימות בסיסי באמצעות curl ואימות SSL](https://stackoverflow.com/questions/26767132/performing-basis-authentication-with-curl-and-ssl-verification)