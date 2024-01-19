---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא דרך להתקשר עם שרת אינטרנט. מתכנתים שולחים בקשות HTTP כדי לגשת למידע, לצורך ביצוע פונקציות, כמו הגשת טפסים, או בדיקת מצב של שרתים.

## איך לבצע:
לנו מספר מודולים בפייתון אשר מאפשרים לנו לשלוח בקשות HTTP. "Requests" הוא אחד הנוחים ביותר לשימוש. הראשון שנעשה זה להתקין אותו ע"י פקודת pip:

```Python
pip install requests
```

ולאחר מכן נבצע את הבקשת HTTP GET:

```Python
import requests

response = requests.get('https://www.example.com')

print(response.text)
```

כאשר נפעיל את הקטע הבא של הקוד, נוכל לראות את תוכן הדף של www.example.com.

## התעמקות:
אינטרנט פועל בעיקר על בקשות HTTP. בשנת 1996 הוא הפך לסטנדרט באינטרנט, ומאז אינספור שיפורים בוצעו לשפה. יתרה מכך, "Requests" היא רק אחת מהספריות שבהן פייתון נתמך, ישנן גם את urllib וhttplib2, חבילות אחרות שאפשרות שליחת הכי הרבה סוגים של בקשות HTTP.

עם ספריית "Requests" (ומרבית ספריות פייתון המקבילות לה), התהליך די פשוט: אתה שולח בקשת HTTP ומקבל תגובה. על פי התגובה הזו, אתה יכול לנתח את הנתונים שקיבלת, לגשת למידע שאתה צריך, או לבצע פעולות נוספות.

## ראה גם:
- [התיעוד הרשמי של ביבליות ה-Requests](https://docs.python-requests.org/en/latest/)
- [מאמר מעולה שמסביר את HTTP באופן מפורט](https://developer.mozilla.org/he/docs/Web/HTTP/Overview)
- [איך לשלוח בקשות HTTP בפייתון באמצעות urllib](https://docs.python.org/3/library/urllib.request.html#module-urllib.request)