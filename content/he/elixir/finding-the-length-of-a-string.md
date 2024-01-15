---
title:                "איתור אורך מחרוזת"
html_title:           "Elixir: איתור אורך מחרוזת"
simple_title:         "איתור אורך מחרוזת"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה
אנשים מחפשים את אורך המחרוזת בכדי להבין את מבנה הנתונים של המחרוזות, לבצע טיפול בטקסט או לתכנת פונקציות שיכולות להתאים את התכונות של מחרוזות ספציפיות.

## כיצד לעשות זאת
```Elixir
string = "האם אתה אוהב אליקסיר?"
IO.puts String.length(string)
```
פלט:
```
18
```

## הכנסה עמוקה
מחרוזת היא סדרה של תווים במחשב. האורך של מחרוזת נמדד על ידי מספר התווים במחרוזת כולל רווחים ותווים מיוחדים. באליקסיר, ניתן למצוא את אורך המחרוזת בעזרת הפונקציה `String.length()`. יש גם אפשרות להשתמש במקוצרת `len()` במקום `String.length()`.

## ראה גם
- [מסמכי הלימוד של אליקסיר](https://elixir-lang.org/getting-started/introduction.html) 
- [דוגמאות קוד עבור אורך מחרוזת](https://github.com/sandoragabriels/elixir-examples/tree/master/string-length)