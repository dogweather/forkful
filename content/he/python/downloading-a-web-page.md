---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ?מה ולמה
הורדת דף אינטרנט היא תהליך של קבלת המידע מדף אינטרנט ושמירתו למחשב המקומי שלך. מתכנתים עשויים לעשות זאת כדי לנתח מידע, לבצע ניתוחים אוטומטיים, או ליצור במידע גיבויים.

## איך לעשות
מטה יש דוגמאות לקוד איך להוריד דף אינטרנט באמצעות Python:

```Python
import requests
res = requests.get('http://www.pythonscraping.com')
res.text
```
מה שקורה כאן הוא שאנחנו משתמשים במודול requests של Python כדי לשלוח בקשה GET לאתר. התגובה מוחזקת באובייקט Response, שאנחנו מדפיסים את התוכן שלו.

## צלילה עמוקה
פעם, החלפנו באופן ידני את הקוד בין מחשבים. כיום, אנחנו יכולים להוריד  דף אינטרנט בצורה אוטומטית באמצעות Python. היתרון הוא כמובן במהירות, יעילות, ובאפשרות לבצע את התהליך בצורה שטחית. 

אופציות נוספות כוללות שימוש ב- `urllib` במקום `requests`, או שימוש במודולים חיצוניים כמו `Scrapy`.

כאשר אנחנו מורידים דף אינטרנט, למעשה מה שאנחנו מבצעים הוא שולחים בקשת GET לשרת האתר המכיל את הדף, מקבלים תגובה מהשרת, ואז מאחסנים את התוכן של התגובה (שהוא למעשה ה-KML של הדף).

## ראו גם
- [Requests: HTTP for Humans™](https://requests.readthedocs.io/en/master/)
- [Scrapy](https://scrapy.org)
- [Python urllib](https://docs.python.org/3/library/urllib.html)
אל תשכחו, אנחנו נמצאים כדי ללמוד ולהשתפר. תמיד חפשו אחר מידע נוסף ודאגו להכיר בטוב את הכלים שלכם!