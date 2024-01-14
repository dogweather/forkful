---
title:                "Elixir: שרשור מחרוזות"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

מדוע לשתף פעולה עם הגיוון בשרשרת מחרוזות יכול להיות שימושי בתכנות ב- Elixir?

## איך לעשות

מתחת לתבנית קוד "```Elixir ... ```" תוכלו למצוא דוגמאות של קוד לשרשור מחרוזות ואת הפלט המתאים.

```Elixir
# דוגמאות לשרשור מחרוזות עם אופרטור הגידול
IO.puts("Hello" <> " " <> "world") # פלט: Hello world

# דוגמאות לשרשור מחרוזות עם פונקציות מובנות
name = "אליבאבא"
IO.puts("שלום" <> String.upcase(name)) # פלט: שלוםאליבאבא

# דוגמאות לשימוש ב-interpolation עבור שרשור מחרוזות
username = "elixir_learner"
IO.puts("שם המשתמש שלי הוא #{username}.") # פלט: שם המשתמש שלי הוא elixir_learner.
```

## טיפול עמוק

בשרשור מחרוזות הינו חלק חשוב מאוד בתכנות ב- Elixir. פעולה זו מאפשרת לנו ליצור מחרוזות חדשות ממחרוזות קיימות, לאחד מחרוזות ומשתנים ולהפעיל פונקציות על מחרוזות בקלות ויעילות. כמו כן, ניתן גם להשתמש בתוכניות ביישומים ובתכניות מתוחכמות לחיבור של מחרוזות באופן אוטמטי.

## ראו גם

- [למדו עוד על השתמשות בתוכניות ביישומים לשרשור מחרוזות](https://hexdocs.pm/elixir/String.Chars.html#operators)
- [סקירה מעמיקה יותר על פונקציות מובנות לשרשור מחרוזות](https://elixir-lang.org/docs/stable/elixir/String.html#module-interpolation)
- [פונקציות מובנות נוספות לעבודה עם מחרוזות ב-Elixir](https://elixir-lang.org/docs/stable/elixir/String.html#module-functions)