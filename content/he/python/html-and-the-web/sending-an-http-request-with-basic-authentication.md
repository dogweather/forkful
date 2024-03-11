---
date: 2024-01-20 18:02:45.323361-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05D8\u05DB\u05E0\u05D9\u05E7\u05D4 \u05DC\u05D2\u05D9\u05E9\u05D4 \u05DE\u05D0\
  \u05D5\u05D1\u05D8\u05D7\u05EA \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9\u05DD \u05D1\
  \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\
  \u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\u05D2\u05D9\u05E9\u05D4\
  \ \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9\u05DD \u05DE\u05D5\u05D2\u05D1\u05DC\u05EA\
  \ \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05E2\u05DD \u05D4\u05E8\u05E9\
  \u05D0\u05D5\u05EA \u05E0\u05DB\u05D5\u05E0\u05D5\u05EA."
lastmod: '2024-03-11T00:14:12.050088-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05D8\u05DB\u05E0\u05D9\u05E7\u05D4 \u05DC\u05D2\u05D9\u05E9\u05D4 \u05DE\u05D0\
  \u05D5\u05D1\u05D8\u05D7\u05EA \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9\u05DD \u05D1\
  \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\
  \u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\u05D2\u05D9\u05E9\u05D4\
  \ \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9\u05DD \u05DE\u05D5\u05D2\u05D1\u05DC\u05EA\
  \ \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05E2\u05DD \u05D4\u05E8\u05E9\
  \u05D0\u05D5\u05EA \u05E0\u05DB\u05D5\u05E0\u05D5\u05EA."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא טכניקה לגישה מאובטחת למשאבים באינטרנט. תוכניתנים עושים את זה כדי לוודא שהגישה למשאבים מוגבלת למשתמשים עם הרשאות נכונות. 

## איך לעשות:
כדי לשלוח בקשה עם אימות בסיסי בפייתון, תוכל להשתמש במודול `requests`. כאן בישור לדבר - דוגמה לקוד לשליחת בקשת GET עם אימות בסיסי:

```Python
import requests
from requests.auth import HTTPBasicAuth

url = 'https://your-api.com/data'
username = 'user123'
password = 'securepassword'

response = requests.get(url, auth=HTTPBasicAuth(username, password))

print(response.status_code)
print(response.json())
```

פלט דוגמה:

```
200
{'data': 'כל מיני נתונים שאתה צריך'}
```

## צלילה עמוקה:
אימות בסיסי בHTTP הוא פשוט ועתיק יומין, תחילה מפורט בRFC 7617. מאחר ואין הצפנה מובנית, הוא נחשב לפחות בטוח בפני מתקפות כמו 'Man in the Middle'. תמיד עדיף לשלבו עם SSL/TLS (HTTPS). חלופות לאימות בסיסי כוללות: אימות מותאם אישית, OAuth ו-API Keys. אחת הבעיות באימות בסיסי היא ששם המשתמש והסיסמה נשלחים בקידוד Base64, אשר קל לפענוחו, לכן שילוב עם HTTPS הוא מומלץ.

## ראה גם:
- תקן RFC 7617: https://tools.ietf.org/html/rfc7617
- מבוא למודול `requests` בפייתון: https://requests.readthedocs.io/en/master/
- מידע נוסף על HTTPS ואבטחת המידע: https://www.eff.org/https-everywhere
