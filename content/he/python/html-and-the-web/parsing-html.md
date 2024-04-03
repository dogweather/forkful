---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:11.117360-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E4\u05D9\u05D9\
  \u05EA\u05D5\u05DF \u05DE\u05E1\u05E4\u05E7\u05EA \u05E1\u05E4\u05E8\u05D9\u05D5\
  \u05EA \u05D7\u05D6\u05E7\u05D5\u05EA \u05DB\u05DE\u05D5 BeautifulSoup \u05D5-requests\
  \ \u05E2\u05D1\u05D5\u05E8 scraping \u05D1\u05E8\u05E9\u05EA \u05D5\u05E0\u05D9\u05EA\
  \u05D5\u05D7 HTML. \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC, \u05EA\u05E6\u05D8\u05E8\
  \u05DA \u05DC\u05D4\u05EA\u05E7\u05D9\u05DF \u05D0\u05EA \u05D4\u05E1\u05E4\u05E8\
  \u05D9\u05D5\u05EA \u05D4\u05DC\u05DC\u05D5 \u05D0\u05DD \u05E2\u05D3\u05D9\u05D9\
  \u05DF \u05DC\u05D0 \u05E2\u05E9\u05D9\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.631742-06:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05DE\u05E1\u05E4\u05E7\u05EA \u05E1\
  \u05E4\u05E8\u05D9\u05D5\u05EA \u05D7\u05D6\u05E7\u05D5\u05EA \u05DB\u05DE\u05D5\
  \ BeautifulSoup \u05D5-requests \u05E2\u05D1\u05D5\u05E8 scraping \u05D1\u05E8\u05E9\
  \u05EA \u05D5\u05E0\u05D9\u05EA\u05D5\u05D7 HTML."
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
weight: 43
---

## איך לעשות:
פייתון מספקת ספריות חזקות כמו BeautifulSoup ו-requests עבור scraping ברשת וניתוח HTML. להתחיל, תצטרך להתקין את הספריות הללו אם עדיין לא עשית זאת:

```bash
pip install beautifulsoup4 requests
```

הנה דוגמה בסיסית שמשתמשת ב-`requests` כדי לצלוף את תוכן ה-HTML של דף אינטרנט וב-`BeautifulSoup` כדי לנתח אותו:

```python
import requests
from bs4 import BeautifulSoup

# צליפת תוכן של דף אינטרנט
URL = 'https://example.com'
page = requests.get(URL)

# ניתוח תוכן ה-HTML
soup = BeautifulSoup(page.content, 'html.parser')

# דוגמה לחילוץ כותרת הדף
title = soup.find('title').text
print(f'כותרת הדף: {title}')
```

**פלט לדוגמה**:
```
כותרת הדף: Example Domain
```

לשאלות מורכבות יותר, כמו לחלץ את כל הקישורים מדף אינטרנט, ניתן להשתמש בשיטות שונות של BeautifulSoup לניווט וחיפוש בעץ הניתוח:

```python
# חילוץ כל הקישורים בתגי <a>
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**פלט לדוגמה**:
```
https://www.iana.org/domains/example
```

גמישותה של BeautifulSoup מאפשרת לך להתאים אישית את חיפושך לנתונים הדרושים בדיוק, מה שהופך את ניתוח HTML לכלי עוצמתי עבור תכניתנים העובדים עם תוכן רשת.
