---
title:                "כתיבת בדיקות"
date:                  2024-01-19
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות היא יצירת סקריפטים הבודקים אם קוד מסוים עובד כראוי. תוכניתנים עושים זאת כדי לוודא אמינות ולהקל על תחזוקת הקוד בעתיד.

## איך לעשות:
```Bash
# שימוש בתוכנית test לבדיקת תנאים
[[ 10 -gt 5 ]] && echo "Yes, 10 is greater than 5"

# פלט דוגמה
Yes, 10 is greater than 5

# בדיקת קיום קובץ
FILE="/etc/passwd"
[[ -f "$FILE" ]] && echo "$FILE exists" || echo "$FILE does not exist"

# פלט דוגמה
/etc/passwd exists
```

## עיון עמוק
החל מהגירסאות הראשונות של UNIX, ניתוח בדיקות נעשה באמצעות סקריפטים של. מאז התפתחו כלים רבים כמו Pytest ב-Python או JUnit ב-Java. ההבדל בין `test` (או `[`) ל-`[[` הוא ש-`[[` הוא מודרני יותר ותומך בתחביר מתקדם יותר כמו && ו-|| לתנאים מרובים.

## ראה גם:
- Bash Guide for Beginners: http://tldp.org/LDP/Bash-Beginners-Guide/html/Bash-Beginners-Guide.html
- Advanced Bash-Scripting Guide: http://tldp.org/LDP/abs/html/
- Google’s Shell Style Guide: https://google.github.io/styleguide/shellguide.html
