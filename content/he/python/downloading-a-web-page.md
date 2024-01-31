---
title:                "הורדת דף אינטרנט"
date:                  2024-01-20T17:44:47.784721-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?

הורדת דף אינטרנט זה פשוט לשמור את התוכן של דף אינטרנט במחשב שלך. תוכניתנים עושים את זה כדי לעבד נתונים, לבצע בדיקות או לארגן מידע באופן אוטומטי.

## איך לעשות:

בואו נשתמש ב-`requests` שזו ספרייה פופולרית לביצוע בקשות HTTP בפייתון.

```Python
import requests

# שליחת בקשת GET לאתר
response = requests.get('https://example.com')

# בדיקה שקיבלנו מענה חיובי
if response.status_code == 200:
    # שמירת התוכן בקובץ
    with open('example.html', 'w', encoding='utf-8') as file:
        file.write(response.text)

# הדפסת התוכן בקונסול לבדיקה
print(response.text)
```

הקוד הזה יוצר קובץ שנקרא `example.html` עם ה-html של אתר `example.com`.

## נסיון עמוק:

הורדת דפי אינטרנט אינה רעיון חדש. במהלך השנים, היה צורך לארגן את האינטרנט ולעשות אותו נגיש יותר באמצעות שיטות שונות, כמו ובים ו-APIs. לפני ש-'requests' התמקמה כסטנדרט, ספריות כמו urllib וhttplib היו צעדים ראשונים לגישה לרשת בפייתון.

## לגבי חלופות:

אם אתה צריך להתמודד עם JavaScript דינמי בדף, `requests` לא יספיק. במקום זאת, תצטרך כלים כמו Selenium או Splash שמדמים דפדפן ויכולים להריץ JavaScript.

## אז מה הלאה?

ביצועים: עבודה עם הספרייה 'requests' היא קלילה, אבל בפרויקטים גדולים עם הרבה בקשות, יתכן שתרצה לבחון את asyncio וaiohttp, שמאפשרות עבודה א-סינכרונית.

## ראו גם:

- מסמכים רשמיים של 'requests': https://requests.readthedocs.io
- Selenium: https://www.selenium.dev
- aiohttp (עבור א-סינכרון): https://docs.aiohttp.org
