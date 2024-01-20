---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פענוח HTML הוא התהליך שבו אנחנו משנים דף אינטרנט למבנה נתונים מאורגנים, כמו עץ DOM. תכנתים עושים את זה כדי לגשת למידע באופן אוטומטי, למניעה של שימוש ממשק משתמש או לניתוח נתונים.

## איך ל:
אחת הספריות הפופולריות ביותר עבור פענוח HTML ב-Python היא BeautifulSoup. הריצו קוד בסיסי כדי לראות את זה בפעולה:

```Python
from bs4 import BeautifulSoup
import requests

url = "https://www.example.com"
page = requests.get(url)

soup = BeautifulSoup(page.content, 'html.parser')
links = soup.find_all('a')

for link in links:
    print(link.get('href'))
```

זה ידפיס את כל הקישורים בדף.

## צלילה עמוקה
- הקשר היסטורי: במקור, HTML נוצר כדי להגדיר מבנה דפי האינטרנט, אך הוא התפתח והתהווה לשפה מורכבת. אתה יכול לראות שינויים במהלך השנים בגרסאות של HTML.
- החלופות: ספריות אחרות פופולריות שתכנתים משתמשים בהן כוללות lxml, pyquery ו־ html5lib. ההחלטה היא בעיקר בראש ובראש תלויה בצרכים ובמשאבים של פרויקטים מסוימים.
- פרטי ביצוע: BeautifulSoup מתאים להמון מטלות פענוח HTML, אך הוא יכול להיות קצת איטי ביחס לספריות כמו lxml בגלל אופן התמחור השונה שלהם.

## ראה גם:
- [מדריך BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [הבדלים בין BeautifulSoup ו- lxml](https://www.azavea.com/blog/2014/03/11/intro-to-virtualenv-and-beautiful-soup/)
- [מדריך לספריית pyquery](https://pythonhosted.org/pyquery/)
- [מדריך ל- html5lib](https://html5lib.readthedocs.io/en/latest/)