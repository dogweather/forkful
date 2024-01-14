---
title:                "Python: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

בזמן שמשתמשים באינטרנט, תהליך של שליחת בקשת HTTP יכול להיות מאוד מומחר וחשוב. כמו כן, שליחת בקשת HTTP היא דרך נוחה ויעילה לתקשורת עם שרתים ולקבלת מידע מהרשת. 

## איך לעשות זאת

```Python
import requests

url = "https://www.example.com"
response = requests.get(url)

# פלט משוער מהקוד הנ"ל:
# 200
# <Response [200]>
```

הדוגמה לעיל מראה כיצד לשלוח בקשת HTTP לכתובת אתר מסוימת ולקבל תגובה. ניתן להשתמש בספריית "requests" בפייתון כדי להפעיל שליחת בקשות HTTP בצורה פשוטה ומתקדמת. בקוד נ"ל אנו משתמשים בפונקציות "get()" ו-"requests", התחביר הנכון יוחזר כתוצאה ויופיע כפלט.

## עובדה עמוקה

שליחת בקשת HTTP היא תהליך מכל הדוחף, מבחינת הצד המשתמש ומבחינת השרת שמתקבל בו הבקשה. לאורך התהליך, המידע נשלח בפורמט קבוע וניתן לעקוב אחריו באמצעות כלי מסוים. חשוב לוודא שהנתנו המשוער מתאים לבקשה שנשלחה וכי כל המידע הנדרש מועבר בצורה נכונה.

## ראה גם

- [הספרייה "requests" בפייתון](https://requests.readthedocs.io/en/latest/)
- [מידע פורמלי על בניית בקשות HTTP](https://www.w3.org/Protocols/rfc2616/rfc2616.html)
- [הבדלים בין HTTP ל-HTTPS](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)