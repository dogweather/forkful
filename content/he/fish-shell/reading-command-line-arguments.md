---
title:                "Fish Shell: קריאת ארגומנטים בשורת פקודה"
simple_title:         "קריאת ארגומנטים בשורת פקודה"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

כדי להשתמש בתכנת פיש ולהיות יכול לנהל ולעקב אחרי תהליך התוכנה שלך באמצעות שורת פקודה בצורה יעילה ומהירה, כדאי לדעת כיצד לקרוא פרמטרי שורת הפקודה.

## איך לעשות זאת

קריאת פרמטרים משורת הפקודה בתכנת פיש יכולה להיות מאתגרת בהתחלה, אבל עם כמה יסודות וקוד מדוגם, תהיה בעלי מיומנויות מעולה בכך. כאן אנו מציגים כמה דוגמאות לקריאת פרמטרים משורת הפקודה בפיש ומציגים את הפלט המתאים לכל דוגמה.

```Fish Shell
set my_var "Hello World"
echo $my_var
```
פלט: "Hello World"

```Fish Shell
echo $argv[1]
```
אם נקרא פקודת הפיש עם הפרמטר "John", הפלט יהיה: "John"

## עומק יותר

כדי להבין טוב יותר כיצד פיש קוראת את פרמטרי שורת הפקודה, כדי לברר אם ישנו פרמטר מסוים או לברר את כמות הפרמטרים שנקלטו, נוכל להשתמש בפקודות נוספות כגון `count` ו-`contains`.

```Fish Shell
count $argv
```
פלט: מספר הפרמטרים שנקלטו

```Fish Shell
if contains $argv[1] 'help' 
    echo "Usage: command_name [options]"
end
```
אם הפרמטר הראשון בפקודת הפיש הינו "help", הפלט יהיה: "Usage: command_name [options]"

## ראה גם

- [תיעוד רשמי של תכנת פיש](https://fishshell.com/docs/current/index.html)
- [למידע נוסף אודות קריאת פרמטרי שורת הפקודה](https://www.cyberciti.biz/faq/unix-linux-shell-scripting-passing-arguments/)