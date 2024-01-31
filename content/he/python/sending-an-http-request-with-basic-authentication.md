---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
date:                  2024-01-20T18:02:45.323361-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"

category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/sending-an-http-request-with-basic-authentication.md"
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
