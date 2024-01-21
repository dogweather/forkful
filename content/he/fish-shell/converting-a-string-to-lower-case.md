---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:39:01.712890-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרה של מחרוזת לאותיות קטנות היא פעולה שמשנה את כל האותיות הגדולות במחרוזת לאותיות קטנות. מתכנתים עושים זאת כדי לאחד פורמטים, לבצע השוואות רגישות לרישיות, ולהתמודד עם קלט משתמש.

## איך לעשות:
הדוגמאות הבאות מדגימות איך להמיר מחרוזות לאותיות קטנות ב- Fish Shell:
```
echo 'HeLLo WoRlD' | string lower
```
פלט לדוגמה:
```
hello world
```
בצעו את הפעולה ישירות על משתנים:
```
set my_string 'SuPeR-CaSe-INsEnSiTiVe'
echo $my_string | string lower
```
פלט לדוגמה:
```
super-case-insensitive
```

## טבילת אש:
הפעולה של המרת אותיות לקטנות קיימת מאז ימי התכנות הראשונים, כאשר התקשורת עם מחשבים הייתה טקסטואלית. ישנן אלטרנטיבות כמו `awk`, `tr`, ובשפות תכנות אחרות. ב-Fish, הפקודה `string lower` היא חלק מהמערכת הפנימית של השפה, מה שאומר שהיא מובנית ומהירה לשימוש. כאשר אתה משתמש בפקודה זו, Fish מפעיל פונקציונליות של עיבוד מחרוזות שמיעלת תכנות טקסטואלי ועיבוד קלט.

## ראה גם:
- [Fish Shell Documentation on `string`](https://fishshell.com/docs/current/cmds/string.html)
- [Stack Overflow: Case conversion in Fish Shell](https://stackoverflow.com/questions/tagged/fish)
- [GNU `tr` Command Manual](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)

קישורים אלו מכילים מקורות קשורים לשימוש בפקודות להמרה של רישיות ולעבודה עם מחרוזות ב-Fish, כמו גם מידע כללי על עיבוד טקסט בשורת הפקודה.