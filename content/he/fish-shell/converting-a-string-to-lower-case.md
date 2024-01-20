---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות אומרת שנשנה את כל האותיות הגדולות במחרוזת לאותיות קטנות. מתכנתים משתמשים בזה כדי לסנן מחרוזות, לבצע השוואות, ולנוע לאמת התרבות שלא משנה האם אותה מחרוזת מוזנה באותיות גדולות או קטנות.

## איך לעשות:
קוד הדגמה של המרת מחרודת לאותיות קטנות בשל Fish:
```fish
 # צור משתנה עם מחרוזת
 set variable 'MyString'

 # מר חרוזת לאותיות קטנות
 echo $variable | string lower
 ```

ההוצאה לפועל של הדוגמה הזו תביא לך "mystring".

## בהקשר רחב יותר
המרת מחרוזות לאותיות קטנות היא גישה מסורתית מימי ה-C. בשפות אחרות, הם עשויים לשנות את המבנה שלו, אך הרעיון שאמור להיות מנוהל בצורה כללית. אם אתה עובד בסביבה שמרת הודעות רוגעות, תוכל להפנות אותן באמצעות שורת הפקודה או כל מנות-מערכת.

## ראה גם:
1. [מדריך לשל Fish](https://fishshell.com/docs/current/index.html)