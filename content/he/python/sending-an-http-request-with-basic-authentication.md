---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP עם אימות בסיסי היא דרך לשלב את הזיהוי שלך בבקשה השולחת. מתכנתים ערים כזאת כדי לדאוג לבטיחות המידע שלהם, תוך השגת הרשאות משתמש מסויימות מהשרת.

## איך ל:

```Python
import requests
from requests.auth import HTTPBasicAuth

def send_request():
    res = requests.get('https://api.github.com/user', auth=HTTPBasicAuth('user', 'pass'))
    if res.status_code == 200:
        return 'Success!'
    else:
        return 'Failed to authenticate.'

print(send_request())
```
הפלט יהיה 'Success!' אם האימות הצליח ו'Failed to authenticate.' אם לא.

## צלילה עמוקה:

אותמן אימות HTTP בייסיק הגיע עם פרט REST API כדרך מוכרת ופשוטה לאמת את הלקוחות. אפשרויות חלופיות יכולות לכלול OAuth ו-API Key, אך שליחת בקשת HTTP עם אימות בסיסי היא דרך אבטחה קלה לשימוש המועדפת על רבים בגלל משקלה הנמוך של ביצועים והפשטות.

## ראו גם:

1. [HTTP Basic Access Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
2. [Python requests library documentation](https://docs.python-requests.org/en/latest/)
3. [REST API Authentication Basics](https://www.youtube.com/watch?v=501dpx2IjGY)