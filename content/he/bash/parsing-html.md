---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/parsing-html.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
פיענוח HTML הוא התהליך שבו ניתן לקרוא ולנתח את קובצי ה-HTML על מנת לקבל מידע. מתכנתים עושים זאת כדי לטפל במידע מקובץ HTML, לשנות אותו או ליצור דוחות.

## איך עושים את זה:
אפשר לפענח את HTML באמצעות XML parser לדוגמא 'xmllint':

```Bash
xmllint --html --xpath '//h2/text()' index.html
```

יכול להיות שתיצטרך להתקין את הכלי `xmllint`. עשה את זה באמצעות:

```Bash
sudo apt-get install libxml2-utils
```

## צנחת העומק:
פענוח HTML הוא לא משהו חדש - הוא היה קיים מאז שה-HTML נוצר. יאתרים אחרים, כמו 'BeautifulSoup' ו-'html.parser' (Python), מספקים אמצעים לפענוח, אבל 'xmllint' הוא כלי תוך-קו שנותן מענה מהיר. במידע והמימוש הטכני שתחתיו, 'xmllint' משתמש בDOM רנדרינג, שמגדיר את הקובץ באופן היררכי של אובייקטים.

## ראה גם:
- xmllint: http://xmlsoft.org/xmllint.html
- BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- html.parser: https://docs.python.org/3/library/html.parser.html