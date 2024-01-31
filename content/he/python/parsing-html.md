---
title:                "ניתוח HTML"
date:                  2024-01-20T15:33:37.391683-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"

category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פיענוח HTML הוא תהליך שבו אנו ממירים מסמך HTML למבנה נתונים שאפשר לנהל ולעבד בשפת תכנות, כמו Python. אנחנו עושים זאת כדי לחלץ מידע, לפרוס ממשקים אינטרנטיים, ולאוטומטיזציה של קטינות ובדיקות אתרי אינטרנט.

## איך לעשות:
על מנת לפרסר HTML בפייתון, אחת הספריות הפופולריות היא BeautifulSoup. קודם כל, ניצור סביבת עבודה וירטואלית ונתקין אותה:

```Python
python -m venv venv
source venv/bin/activate
pip install beautifulsoup4
```

דוגמא לקוד פרסור:

```Python
from bs4 import BeautifulSoup

html_doc = """
<html><head><title>The Dormouse's story</title></head>
<body>
<p class="title"><b>The Dormouse's story</b></p>

<p class="story">Once upon a time there were three little sisters; and their names were
<a href="http://example.com/elsie" class="sister" id="link1">Elsie</a>,
<a href="http://example.com/lacie" class="sister" id="link2">Lacie</a> and
<a href="http://example.com/tillie" class="sister" id="link3">Tillie</a>;
and they lived at the bottom of a well.</p>

<p class="story">...</p>
"""

# האובייקט soup מייצג את הדף
soup = BeautifulSoup(html_doc, 'html.parser')

# קבלת טקסט הכותרת
title_text = soup.title.string
print(f"The title of the story is: {title_text}")

# חיפוש כל הקישורים בדף (tags של <a>)
links = soup.find_all('a')
for link in links:
    print(link.get('href'))
```

פלט הדוגמה:

```
The title of the story is: The Dormouse's story
http://example.com/elsie
http://example.com/lacie
http://example.com/tillie
```

## צלילה לעומק:
פרסור HTML אינו חידוש, וכבר בראשית ימי האינטרנט היו כלים לכך. BeautifulSoup היא ספרייה שנכתבה ב-2004 והפכה לסטנדרט בפייתון לפרסור HTML ו-XML. יחד עם ספריות כמו lxml ו-html.parser, היא מאפשרת גישה נוחה וגמישה לניתוח מסמכים אלו.

היתרון של BeautifulSoup הוא שהיא עמידה גם בפני HTML "שבור" – דפים שלא מתאימים בדיוק למפרט. זה מאפשר לה לעבוד גם על אתרים עם קוד לא מסודר או ישן.

קיימים גם כלים אחרים כמו Scrapy, שהם יותר ממערכת גריפינג מלאה, אבל עבור פרויקטים קטנים ומטלות פשוטות - BeautifulSoup מספיקה בהרבה מקרים.

## ראה גם:
- [מדריך ל-BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [מדריך רשמי לספריית lxml](https://lxml.de/)
- [מסמך קידוד עברית בהיסטוריה של HTML5](https://www.w3.org/International/questions/qa-html-dir)
- [Scrapy, פריימוורק לגריפינג](https://scrapy.org/)
