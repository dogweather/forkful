---
date: 2024-01-20 18:04:00.889055-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9 \u05D1-Elixir, \u05EA\u05E9\u05EA\u05DE\u05E9\u05D5 \u05D1-Mix, \u05DB\
  \u05DC\u05D9 \u05D4\u05E0\u05D9\u05D4\u05D5\u05DC \u05E9\u05DC Elixir."
lastmod: '2024-03-13T22:44:38.774509-06:00'
model: gpt-4-1106-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC \u05E4\u05E8\u05D5\
  \u05D9\u05E7\u05D8 \u05D7\u05D3\u05E9 \u05D1-Elixir, \u05EA\u05E9\u05EA\u05DE\u05E9\
  \u05D5 \u05D1-Mix, \u05DB\u05DC\u05D9 \u05D4\u05E0\u05D9\u05D4\u05D5\u05DC \u05E9\
  \u05DC Elixir."
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
weight: 1
---

## איך לעשות:
כדי להתחיל פרויקט חדש ב-Elixir, תשתמשו ב-Mix, כלי הניהול של Elixir.

```elixir
# התקנת Elixir
$ sudo apt-get install elixir

# יצירת פרויקט חדש
$ mix new my_project

# Output:
* creating README.md
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/my_project.ex
* creating test
* creating test/test_helper.exs
* creating test/my_project_test.exs

# כדי לרוץ את הטסטים:
$ cd my_project
$ mix test

# Output:
....

Finished in 0.03 seconds
1 test, 0 failures

Randomized with seed 54321
```

## עיון מעמיק:
ה-Mix הוא מערכת ניהול פרויקטים ומשימות שמגיעה עם Elixir מהגרסה 1.0. הוא מספק סביבה מתוחזקת ליצירת, קומפילציה, וניהול תלותים. לפני Mix, פרויקטי Elixir היו דורשים עבודה ידנית רבה יותר. עם מעבר הזמן, ה-Mix פיתח פונקציונליות שפשטה עוד יותר את הפתיחה והניהול של פרויקטים חדשים. אלטרנטיבת Mix בעולם רובי היא ה-Bundler, ובעולם ג'אווה – Maven או Gradle. לעומתם, Mix משלב כלי ניהול תלותים בנוחות של שורת פקודה אחת ובהתממשקות עם סביבת ה-OTP של ארלנג.

## ראו גם:
- [Elixir רשמי – התחלה עם Mix](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
- [Elixir School – ניהול תלותים](https://elixirschool.com/en/lessons/basics/mix/)
- [Elixir Forum](https://elixirforum.com/) – קהילת תכנות שבה אפשר לדון ולשאול שאלות על פרויקטים ו-Issues.
