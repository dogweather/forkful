---
title:                "Elixir: חילוץ תת-מחרוזות"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה

אנחנו משתמשים בזיכרויות תת-מחרוזת כדי למצוא מידע מסוים במחרוזת גדולה יותר. למשל, אם אנחנו מנסים למצוא את כמה היום הופיע במחרוזת שתקבל ממשתמש, אנו יכולים לחלץ תת-מחרוזת שמייצגת את היום ולבדוק אם היא תואמת את הקלט. לכן, ייתכן שנצטרך לחלץ תת-מחרוזת כדי לפתור בעיות פשוטות או לעבוד עם מידע מסוים במחרוזת.

## איך לעשות זאת

כדי לחלץ תת-מחרוזת ב- Elixir, אנחנו משתמשים בפונקציית `slice/2`. נעביר לה את המחרוזת המקורית ואת התחילת והסוף של התת-מחרוזת שאנחנו רוצים לחלץ. לדוגמה:

```Elixir
message = "Hello, world!"
greeting = slice(message, 0, 5)

IO.puts greeting # "Hello"
```

ניתן גם להשתמש בפונקציות נוספות כמו `contains?` ו- `starts_with?` כדי לבדוק אם תת-מחרוזת מסוימת קיימת בתוך מחרוזת מסוימת.

## חקירה מעמיקה

בנוסף לחלץ תת-מחרוזות פשוטות, ישנן גם עולמיים שלמים שניתן לגלות באמצעות פונקציות כמו `split`, `trim`, ו- `replace`. אפילו ניתן למצוא תת-מחרוזות בעזרת פונקציות מתקדמות יותר כמו `regex`.

## ראה גם

- תיעוד רשמי של Elixir על פונקציות לעבוד עם מחרוזות: https://hexdocs.pm/elixir/String.html
- מדריך לחלץ תת-מחרוזות בעזרת רגקס ב-Elixir: https://elixirschool.com/blog/thats-so-meta/
- אמולטור מקוון לניסוח רגקס ב-Elixir: https://regex101.com/library/u5X8h1