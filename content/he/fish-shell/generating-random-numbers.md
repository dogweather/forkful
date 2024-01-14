---
title:                "Fish Shell: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

Hebrew:

## למה

למה כדאי להשתמש בתכנות השורה של Fish Shell כדי ליצור מספרים אקראיים?

## איך לעשות זאת

תוכלו ליצור מספרים אקראיים באמצעות כמה פקודות של Fish Shell. ניתן להשתמש בפקודת `math` כדי לקבל מספרים אקראיים בטווח שנקבע על ידי משתנים. לדוגמה:

```
Fish Shell: math random 1..100
Output: 50
```

אם תרצו, תוכלו גם להשתמש בפקודת `seq` כדי ליצור רשימה של מספרים אקראיים בטווח שנקבע. לדוגמה:

```
Fish Shell: seq (math random 1..10)
Output: 3 5 2 10 4 6 8 9 1 7
```

## חקירה מעמיקה

התכונה של Fish Shell שמאפשרת לנו ליצור מספרים אקראיים היא קרובה לכל מה שנקרא "הגנטיקה לינארית". זה אומר שיש לנו פקודות שמאפשרות לנו ליצור מספרים חדשים מהבריחות של מספרים קיימים במכשירים המחשבים שלנו. כדי להבין טוב יותר את הנושא, כדאי לחקור עוד על תכנות השורה של Fish Shell ותכונותיה המתקדמות.

## ראה גם

- [דיון על תכנות השורה של Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [מדריך לפקודת `math` בספריית התמיכה של Fish Shell](https://fishshell.com/docs/current/cmds/math.html)
- [הסבר על פקודת `seq` ושימושיה האפשריים](https://fishshell.com/docs/current/cmds/seq.html)