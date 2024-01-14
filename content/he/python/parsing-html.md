---
title:                "Python: פיענוח html"
simple_title:         "פיענוח html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/parsing-html.md"
---

{{< edit_this_page >}}

## למה

קוראים בכוונה שימושיים של HTML כשאנו חולקים את התכנים שלנו בעזרת אינטרנט. פייתון מאפשר בקלות למיון את המידע הזה בצורה יעילה עם כלי עיבוד HTML.

## כיצד

```Python
# חילוץ תוכן טקסט המופיע בתוך תגיות <p> באתר
from bs4 import BeautifulSoup

# כתיבת הקוד של הדף המטריו
html_doc = """
<html>
<head>
  <title>זוהי הדף הראשי שלי</title>
</head>
<body>
  <h1>שלום רב!</h1>
  <p>זהו הקוד של פייתון.</p>
  <p>זהו קוד לדגמה שימושי.</p>
</body>
</html>
"""

# חילוץ תוכן הטקסט המופיע בתוך תגיות <p>
soup = BeautifulSoup(html_doc, 'html.parser')
paragraphs = soup.find_all('p')

# הדפסת כל תוכן הטקסט המצוין בתוך תגיות <p>
for p in paragraphs:
    print(p.text)
```

פלט:

```
זהו הקוד של פייתון.
זהו קוד לדגמה שימושי.
```

## צליל עמוק

זהו נושא מעניין וחשוב בשפת תכנות פייתון. פיענוח HTML בעזרת פייתון מאפשר לנו להשתמש בתכנים המפורסמים באינטרנט בצורה מתודולוגית ויעילה. הכלים הקיימים בפייתון כמו BeautifulSoup מספקים לנו עצמים חזקים שנותנים לנו את האפשרות לצמצם את הביצועים בקוד ולהתמקד בעבודה שמאחורי התכנים המופיעים באתר.

## ראו גם

- [שפת תכנות פייתון](https://www.python.org/)
- [מדריך BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [תיעוד HTML](https://developer.mozilla.org/he/docs/Web/HTML)