---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות הוא פעולה שבה מביאים מחרוזת מביצעים עליה פעולה, ומקבלים חלק ממנה. תכנתים עושים את זה כדי למנוע חוזר חלילה על קוד או לתפקיד עיבוד מידע.

## איך:
בעזרת סקריפט Fish Shell, ניתן לחלץ תת-מחרוזות בקלות.

```
echo "Hello, World!" | string sub -s 6 -l 5
```
הפלט יהיה "World".

## צלילה עמוקה:
הפקודה string sub הגיעה בגרסה 2.3 של Fish Shell, התכנה שהשיפרה את התמיכה בעיבוד מחרוזת. בדיקה אחרת שאנו יכולים לבצע היא להשתמש בפקודה cut שהיא כלי חד שרירי שמגיע עם UNIX. לחליפה, אנו יכולים גם להשתמש ביכולת ה-bash של `${string:start:length}` על פני מספר של אפשרויות.
חשוב לדעת שהספרת הראשונה ב- Fish Shell שלנו היא 1, לעומת 0 באסמבל, מה שמקל על חילוץ תת-מחרוזות.

## ראו גם:
1. [הדוקומנטציה הרשמית של Fish Shell למחרוזת](https://fishshell.com/docs/current/commands.html#string)
2. [מדריך לשפת תכנות Fish Shell](https://github.com/jorgebucaran/fish-cookbook)
3. [פוסט בפורום Stackoverflow אודות חליצת תת-מחרוזות ב-Fish Shell](https://stackoverflow.com/questions/428109/extract-substring-in-bash)