---
title:                "שליחת בקשת http"
html_title:           "Python: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

ככל שהעולם התקשרות מתקדם, יותר ויותר נדרשת אפשרות לשליחת בקשות HTTP. זהו הדרך העיקרית לתקשורת בין אתרים ואפליקציות שונות וזה חלק חשוב מאוד בקבלת נתונים ושליחת מידע מעל האינטרנט.

## איך לעשות זאת

השליחה של בקשת HTTP בפייתון נעשית באמצעות מודול ה `requests` ותלויה בשיטת השליחה הרצויה (get, post, put, delete וכו'). הנה דוגמא לשליחת בקשה GET לאתר של Google, כולל ביצוע הבקשה והדפסת התוצאה:

```Python
import requests

r = requests.get('https://www.google.com/')
print(r.text)
```
output: <!doctype html><html itemscope="" itemtype="http://schema.org/WebPage" lang="he"><head><meta charset="UTF-8">

## דפיקה עמוקה

בשליחת בקשת HTTP ישנם עוד כמה שיקולים שחשוב לקחת בחשבון. ניתן להוסיף מידע נוסף בכותרת (headers) של הבקשה, לשלוח פרמטרים כחלק מהבקשה, וכן ניתן לסנכרן עם התגובה המשובצת. למשל, ניתן לבדוק את קוד המצב (status code) של התגובה כדי לוודא שהבקשה נשלחה בהצלחה ולטפל בכל מקרה שבו היא נכשלה.

## ראה גם

- עמוד המסמך הרשמי של `requests` מפירוש נושאים נוספים כמו מבנה הבקשה והתגובה.
- [מדריך לשליחת בקשות HTTP בפייתון](https://realpython.com/python-requests/).