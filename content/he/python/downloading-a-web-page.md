---
title:                "הורדת עמוד אינטרנט"
html_title:           "Python: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה
כל אחד יעשה שימוש בהורדת דף אינטרנט בכדי לקבל מידע מסוים מאתר אינטרנט או לבצע פעולות כגון ניתוח נתונים או יצירת ממשק משתמש מתאים.

## איך להוריד דף אינטרנט בפייתון
```python
import requests

# ייבוא הספריה requests
url = 'https://www.example.com' # הגדרת כתובת הדף הרצוי
response = requests.get(url) # בקשת הגעה לכתובת האתר
print(response.text) # הדפסת תוכן הדף
```

כאן אנו משתמשים בספריה של requests לשלוח בקשת GET לכתובת שצוינה, ולאחר מכן מדפיסים את תוכן הדף באמצעות הפקודה `response.text`.

## מעמקים
בנוסף ליכולת להוריד דף אינטרנט מכתובת קיימת, פייתון מאפשר גם לבנות באופן דינמי כתובת ולטעון תוכן של דף אינטרנט חדש. כמו כן, ניתן להשתמש בספריה נוספת כמו BeautifulSoup לניתוח ודפדוף בתוך תוכן הדף המורד.

## ראו גם
- [מדריך על שימוש בספריה requests בפייתון](https://requests.readthedocs.io/en/master/user/quickstart/#make-a-request)
- [תיעוד רשמי על ספריה BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)