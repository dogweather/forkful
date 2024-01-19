---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Bash: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?

בדיקה אם ספרייה קיימת היא מהלך קוד שבו התוויה בודקת אם ספרייה מסוימת כבר קיימת במערכת. זה זה נחוץ למנות כדי למנוע פעולות בלתי רצויות, כמו יצירת ספריות כפולות או שגיאות עת מניחים שספרייה קיימת כאשר היא לא.

## איך לעשות:

באמצעות בדיקת תנאי עם הפקודה `if`, אפשר לבחון בקלות אם ספרייה קיימת. לחלופין, אפשר להשתמש בפקודה `test`.

```Bash
# בודקים אם הספרייה '/my-dir' קיימת
if [ -d "/my-dir" ]; then
  echo "The directory exists."
else
  echo "The directory does not exist."
fi
```
הקוד הזה מדפיס "הספרייה קיימת" אם הספרייה '/my-dir' קיימת, ו"הספרייה לא קיימת" אם אינה קיימת.

## צלילה עמוקה:

בהיסטוריה, הפקודה `test` שומשה בתנאי, אך בהמשך הכניסו מעטפת בניסיון להפוך את התנאים לנוחים יותר. אם מעדיפים את המראה הקלאסי, אפשר להשתמש ב-`test`:

```Bash
test -d "/my-dir" && echo "The directory exists." || echo "The directory does not exist."
```
הקוד הזה יעשה בדיוק אותו דבר. הפקודה `test` עצמה זו כלי יעיל, ובמקרים מסוימים עדיף לזה שלמעלה.

## ראה גם:

[חוברת GNU Coreutils `test`](https://www.gnu.org/software/coreutils/manual/html_node/test-invocation.html)
[מדריך תכנות Bash](https://www.tldp.org/LDP/abs/html/)
[מדריך בנושא בדיקת חלקי קוד](https://ryanstutorials.net/bash-scripting-tutorial/bash-if-statements.php)