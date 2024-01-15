---
title:                "כתיבת מחרוזת באותיות גדולות"
html_title:           "Fish Shell: כתיבת מחרוזת באותיות גדולות"
simple_title:         "כתיבת מחרוזת באותיות גדולות"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה
בעולם התכנות נמצאים בריבוי אנשים שמעוניינים ללמוד טכנולוגיות חדשות ולראות כיצד ניתן ליישם אותן בפועל. קפיטליזציה של מחרוזת הינה כלי חשוב ושימושי בתכנות כדי להפוך את המחרוזת למאוחר ביחס למחרוזת המקורית.

## איך לעשות
הנה דוגמא לקוד שיכול להפוך שם משתנה לצורת קפיטל:
```Fish Shell
set my_name "avi"
echo (string capitalize $my_name)
```
הפלט של קוד זה יהיה "Avi".

## Deep Dive
בפיש של גרסת הנוכחית ישנם מספר דרכים לבצע קפיטליזציה של מחרוזת. בין האפשרויות ניתן להשתמש בפונקציה המובנית tostring או להשתמש במספר תחבירים כמו "&" והאופרטור capitalize. כדי ללמוד עוד על האפשרויות השונות, ניתן לעיין במדריך הרשמי של פיש או לחפש מידע נוסף באינטרנט.

## ראה גם
- [קפיטליזציה - ויקיפדיה](https://he.wikipedia.org/wiki/%D7%A7%D7%A4%D7%99%D7%98%D7%9C%D7%99%D7%96%D7%A6%D7%99%D7%94)
- [מדריך רשמי של פיש של אפשרויות קפיטליזציה](https://fishshell.com/docs/3.3/cmds/capitalize.html)
- [פוסט בבלוג שמסביר היטב קפיטליזציה בפיש](https://blog.alejandro-rayon.com/translations/capitalize-strings-in-fish-shell-in-5-ways/)