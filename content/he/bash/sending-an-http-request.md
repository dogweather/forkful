---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP היא דרך בה מחשב מבקש מידע משרת. תכנתים משתמשים בבקשות HTTP על מנת לאפשר תקשורת בין שרתים ומחשבים, לאחזר מידע מבסיסי נתונים ועוד.

## איך לעשות:

ב-Bash, אפשר לשלוח בקשת HTTP באמצעות הפקודה `curl`. לדוגמה:

```Bash
curl https://www.example.com
```

הפלט הבא הוא התוכן שהשרת מחזיר:

```Bash
<html>
<head><title>ראשי</title></head>
<body>
ברוכים הבאים לאתר שלנו!
</body>
</html>
```

## מעמיקים:

בקשות HTTP אינן חדשות. הן הוצבו לראשונה בהוספת HTTP לרשת האינטרנט ב-1991. ישנם אלטרנטיבות אחרות לביצוע בקשות HTTP ב-Bash, כמו גם `wget`. אפשר לשלב בראשי תיבות בבקשות HTTP (כמו GET ו POST) בסביבת Bash אם נרצה לשלוח נתונים עם הבקשה שלנו.

## ראה גם:

- [דוקומנטציה של `curl`](https://curl.haxx.se/docs/manual.html)
- [מדריך להעברת HTTP](https://developer.mozilla.org/he/docs/Web/HTTP/Overview)
- [ספר בנושא שליחת בקשות HTTP ב-Bash](https://www.shellscript.sh/ftp.html)