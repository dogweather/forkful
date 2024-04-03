---
date: 2024-01-20 18:01:20.547658-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05EA \u05D4\u05E2\u05D1\u05E8\u05EA \u05E9\u05DD \u05DE\u05E9\u05EA\
  \u05DE\u05E9 \u05D5\u05E1\u05D9\u05E1\u05DE\u05D4 \u05D1\u05E6\u05D5\u05E8\u05D4\
  \ \u05DE\u05D0\u05D5\u05D1\u05D8\u05D7\u05EA \u05DC\u05D0\u05EA\u05E8 \u05DB\u05D3\
  \u05D9 \u05DC\u05E7\u05D1\u05DC \u05D2\u05D9\u05E9\u05D4. \u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05D0\u05DE\u05EA \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05E1\
  \u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05D5\u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\
  \u05DD\u2026"
lastmod: '2024-03-13T22:44:39.624241-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05EA \u05D4\u05E2\u05D1\u05E8\u05EA \u05E9\u05DD \u05DE\u05E9\u05EA\
  \u05DE\u05E9 \u05D5\u05E1\u05D9\u05E1\u05DE\u05D4 \u05D1\u05E6\u05D5\u05E8\u05D4\
  \ \u05DE\u05D0\u05D5\u05D1\u05D8\u05D7\u05EA \u05DC\u05D0\u05EA\u05E8 \u05DB\u05D3\
  \u05D9 \u05DC\u05E7\u05D1\u05DC \u05D2\u05D9\u05E9\u05D4."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
weight: 45
---

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
