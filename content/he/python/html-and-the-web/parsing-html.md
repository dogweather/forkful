---
title:                "פיענוח HTML"
aliases:
- /he/python/parsing-html.md
date:                  2024-02-03T19:13:11.117360-07:00
model:                 gpt-4-0125-preview
simple_title:         "פיענוח HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
ניתוח HTML מערב ניתוח קוד ה-HTML של דף אינטרנט כדי לחלץ מידע או אלמנטים מסוימים, משימה נפוצה עבור scraping באינטרנט, כריית נתונים, או אוטומציה של אינטראקציות עם אתרי אינטרנט. תכניתנים עושים זאת כדי להתממשק תכנותית עם אתרי אינטרנט או לחלץ מהם נתונים, לאוטמט פעולות או לבדוק אפליקציות רשת.

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
