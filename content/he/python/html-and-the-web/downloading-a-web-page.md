---
date: 2024-01-20 17:44:47.784721-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05D5\u05D0\
  \u05D5 \u05E0\u05E9\u05EA\u05DE\u05E9 \u05D1-`requests` \u05E9\u05D6\u05D5 \u05E1\
  \u05E4\u05E8\u05D9\u05D9\u05D4 \u05E4\u05D5\u05E4\u05D5\u05DC\u05E8\u05D9\u05EA\
  \ \u05DC\u05D1\u05D9\u05E6\u05D5\u05E2 \u05D1\u05E7\u05E9\u05D5\u05EA HTTP \u05D1\
  \u05E4\u05D9\u05D9\u05EA\u05D5\u05DF."
lastmod: '2024-03-13T22:44:38.633345-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05D5\u05D0\u05D5 \u05E0\u05E9\u05EA\u05DE\u05E9 \u05D1-`requests`\
  \ \u05E9\u05D6\u05D5 \u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05E4\u05D5\u05E4\u05D5\
  \u05DC\u05E8\u05D9\u05EA \u05DC\u05D1\u05D9\u05E6\u05D5\u05E2 \u05D1\u05E7\u05E9\
  \u05D5\u05EA HTTP \u05D1\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
weight: 42
---

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
