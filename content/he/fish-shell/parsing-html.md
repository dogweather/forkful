---
title:                "פיענוח HTML"
aliases:
- he/fish-shell/parsing-html.md
date:                  2024-02-03T19:12:46.043912-07:00
model:                 gpt-4-0125-preview
simple_title:         "פיענוח HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

פיענוח HTML הוא על אודות הוצאת נתונים או מידע מתוכן HTML, משימה נפוצה בעת התמודדות עם נתוני אינטרנט. תכנתים עושים זאת כדי לאוטמט את הוצאת המידע מאתרי אינטרנט, למשימות כמו שרטוט אינטרנט, חציבת נתונים או בדיקה אוטומטית.

## איך לעשות:

בעיקר, קליפת Fish אינה מיועדת לפיענוח HTML באופן ישיר. עם זאת, היא מצטיינת בהדבקת כלים של Unix כמו `curl`, `grep`, `sed`, `awk`, או שימוש בכלים מתמחים כמו `pup` או `beautifulsoup` בתסריט Python. להלן דוגמאות הממחישות איך לנצל את הכלים האלה מתוך קליפת Fish כדי לפענח HTML.

### שימוש ב-`curl` ו-`grep`:
להוציא תוכן HTML ולחלץ שורות המכילות קישורים:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

פלט:
```
/page1.html
/page2.html
...
```

### שימוש ב-`pup` (כלי שורת פקודה לפיענוח HTML):

ראשית, ודא ש-`pup` מותקן. לאחר מכן תוכל להשתמש בו לחלץ אלמנטים לפי התגיות, ids, כיתות וכולי.

```fish
curl -s https://example.com | pup 'a attr{href}'
```

הפלט, דומה לדוגמה של `grep`, היה מפרט את מאפייני href של תגיות `<a>`.

### באמצעות תסריט Python ו-`beautifulsoup`:

למרות ש-Fish עצמה לא יכולה לפענח HTML באופן טבעי, היא משתלבת ללא תקלות עם תסריטי Python. להלן דוגמה תמציתית המשתמשת ב-Python עם `BeautifulSoup` כדי לפרש ולחלץ כותרות מ-HTML. ודא ש-`beautifulsoup4` ו-`requests` מותקנים בסביבת ה-Python שלך.

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

שימוש:

```fish
parse_html 'https://example.com'
```

פלט:
```
Example Domain
```

כל אחת מהשיטות הללו משרתת מקרי שימוש ורמות שונות של מורכבות, החל מניפולציה פשוטה של טקסט בשורת הפקודה ועד לכוח הפיענוח המלא של `beautifulsoup` בתסריטי Python. בהתאם לצרכים שלך ולמורכבות של מבנה ה-HTML, תוכל לבחור בגישת Unix הישירה או בגישת תסריט עוצמתית יותר.
