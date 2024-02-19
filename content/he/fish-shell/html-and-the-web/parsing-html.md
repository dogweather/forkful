---
aliases:
- /he/fish-shell/parsing-html/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:46.043912-07:00
description: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML \u05D4\u05D5\u05D0 \u05E2\u05DC\
  \ \u05D0\u05D5\u05D3\u05D5\u05EA \u05D4\u05D5\u05E6\u05D0\u05EA \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D0\u05D5 \u05DE\u05D9\u05D3\u05E2 \u05DE\u05EA\u05D5\u05DB\
  \u05DF HTML, \u05DE\u05E9\u05D9\u05DE\u05D4 \u05E0\u05E4\u05D5\u05E6\u05D4 \u05D1\
  \u05E2\u05EA \u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\u05D5\u05EA \u05E2\u05DD \u05E0\
  \u05EA\u05D5\u05E0\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05D0\u05D5\u05D8\u05DE\u05D8 \u05D0\u05EA \u05D4\u05D5\u05E6\
  \u05D0\u05EA \u05D4\u05DE\u05D9\u05D3\u05E2 \u05DE\u05D0\u05EA\u05E8\u05D9\u2026"
lastmod: 2024-02-18 23:08:53.292852
model: gpt-4-0125-preview
summary: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML \u05D4\u05D5\u05D0 \u05E2\u05DC\
  \ \u05D0\u05D5\u05D3\u05D5\u05EA \u05D4\u05D5\u05E6\u05D0\u05EA \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D0\u05D5 \u05DE\u05D9\u05D3\u05E2 \u05DE\u05EA\u05D5\u05DB\
  \u05DF HTML, \u05DE\u05E9\u05D9\u05DE\u05D4 \u05E0\u05E4\u05D5\u05E6\u05D4 \u05D1\
  \u05E2\u05EA \u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\u05D5\u05EA \u05E2\u05DD \u05E0\
  \u05EA\u05D5\u05E0\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05D0\u05D5\u05D8\u05DE\u05D8 \u05D0\u05EA \u05D4\u05D5\u05E6\
  \u05D0\u05EA \u05D4\u05DE\u05D9\u05D3\u05E2 \u05DE\u05D0\u05EA\u05E8\u05D9\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
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
