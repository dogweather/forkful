---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
date:                  2024-01-20T18:01:20.547658-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי מאפשרת העברת שם משתמש וסיסמה בצורה מאובטחת לאתר כדי לקבל גישה. תכנתים עושים זאת כדי לאמת משתמשים בסקריפטים ויישומים שמתבצעים מהשרת או מה-terminal.

## איך לעשות:
שליחת בקשה ישירות מה-terminal באמצעות `curl` עם שם משתמש וסיסמה:

```Bash
curl -u username:password http://example.com
```

הצפנת הסיסמה עם Base64 והכנסתה לבקשה עם Header מותאם:

```Bash
ENCODED=$(echo -n 'username:password' | base64)
curl -H "Authorization: Basic $ENCODED" http://example.com
```

דוגמא לפלט שעשוי להופיע לאחר בקשת HTTP מוצלחת:

```
<html>
<head><title>An Example Page</title></head>
<body>
<p>Welcome to our website!</p>
</body>
</html>
```

## טבילה עמוקה
שיטת האימות הבסיסי עברה הרבה מחזורי חיים מאז התחלת האינטרנט. היא פשוטה ונפוצה אך לא הכי מאובטחת בהשוואה לתקנים חדשים כמו OAuth. ברמת המימוש, השם והסיסמה מועברים ב-Header של הבקשה אחרי הצפנה עם Base64. זכרו, Base64 אינו הצפנה בטוחה; הוא רק קידוד. לכן, תמיד משתמשים ב-SSL/TLS בשילוב עם בקשה מוצפנת כדי לשמור על בטיחות הנתונים.

## ראה גם:
1. [cURL Documentation](https://curl.se/docs/)
2. [RFC 7617: The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
3. [Mozilla Developer Network - HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
4. [Base64 Encoding](https://en.wikipedia.org/wiki/Base64)
