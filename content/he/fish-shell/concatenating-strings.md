---
title:                "Fish Shell: סיבוב מחרוזות"
simple_title:         "סיבוב מחרוזות"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

תחברו לתוכנית ציבורית לגירסה האחרונה
של פיש של תחברו תווים. תחברו תווים כדי ליצור מחרוזות מרתמים וליישם אותן בתוכניות שונות.

## איך לעשות

כדי לחבר מחרוזות תוכלו להשתמש בפונקציה `string join` בתוך סביבת פיש.
הנה דוגמאות לקוד שימחבר מחרוזות לתוך משתנים חדשים וימהיר נותני היתר ליישם אותם בתוכניות שלכם:

```fish
set name1 "אבי"
set name2 "רותם"
set name3 "מירי"

set names (string join " ו" $name1 $name2 $name3)
echo $names

# פלט:
# אבי ורותם ומירי
```

פנקציות נוספות כמו `string split` ו`string replace` יכולות לסייע לכם לפתור בעיות נוספות עם מחרוזות. לדוגמה:

```fish
set my_string "טקסט מעניין"
set my_string_split (string split " " $my_string)
echo $my_string_split[2]

# פלט:
# מעניין
```

## עומק

חבירת מחרוזות הינו תהליך בסיסי בתכנות ויכול להיות שימושי בעשרות דרכים שונות. כאשר מחברים מחרוזות, חשוב לקחת בחשבון את הסדר והפרדה של התווים. כמו כן, יש לשמור על הכתיב הללו כדי לוודא שהפונקציות של פיש תעבוד כראוי. ניתן למצוא עוד מידע על פעולות מחרוזות בתיעוד הרשמי של פיש.

## ראו גם

- [דוגמה: חיבור מחרוזות עם פיש](https://fishshell.com/docs/current/cmds/string.html)
- [תיעוד פעולות מחרוזות בפיש](https://fishshell.com/docs/current/cmds/string.html)