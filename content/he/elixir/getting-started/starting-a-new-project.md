---
date: 2024-01-20 18:04:00.889055-07:00
description: "\u05D1\u05DB\u05DC \u05E4\u05E2\u05DD \u05E9\u05DE\u05EA\u05D7\u05D9\
  \u05DC\u05D9\u05DD \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\u05D3\u05E9 \u05D1\
  -Elixir, \u05DE\u05E0\u05E6\u05DC\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05D1\u05E0\u05D5\u05EA \u05DE\u05D1\u05E0\u05D4 \u05D4\u05DE\u05D0\u05E8\
  \u05D2\u05DF \u05D0\u05EA \u05D4\u05E7\u05D5\u05D3 \u05D5\u05DE\u05D0\u05E4\u05E9\
  \u05E8 \u05EA\u05D7\u05D6\u05D5\u05E7\u05D4 \u05D8\u05D5\u05D1\u05D4 \u05D9\u05D5\
  \u05EA\u05E8. \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8\u05D9\u05DD \u05D7\u05D3\u05E9\
  \u05D9\u05DD \u05DE\u05E1\u05D9\u05D9\u05E2\u05D9\u05DD \u05D1\u05D4\u05D1\u05E0\
  \u05D4 \u05D8\u05D5\u05D1\u05D4 \u05D9\u05D5\u05EA\u05E8 \u05E9\u05DC \u05D4\u05E6\
  \u05E8\u05DB\u05D9\u05DD\u2026"
lastmod: 2024-02-19 22:04:58.050364
model: gpt-4-1106-preview
summary: "\u05D1\u05DB\u05DC \u05E4\u05E2\u05DD \u05E9\u05DE\u05EA\u05D7\u05D9\u05DC\
  \u05D9\u05DD \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\u05D3\u05E9 \u05D1-Elixir,\
  \ \u05DE\u05E0\u05E6\u05DC\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05D1\u05E0\u05D5\u05EA \u05DE\u05D1\u05E0\u05D4 \u05D4\u05DE\u05D0\u05E8\u05D2\
  \u05DF \u05D0\u05EA \u05D4\u05E7\u05D5\u05D3 \u05D5\u05DE\u05D0\u05E4\u05E9\u05E8\
  \ \u05EA\u05D7\u05D6\u05D5\u05E7\u05D4 \u05D8\u05D5\u05D1\u05D4 \u05D9\u05D5\u05EA\
  \u05E8. \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8\u05D9\u05DD \u05D7\u05D3\u05E9\u05D9\
  \u05DD \u05DE\u05E1\u05D9\u05D9\u05E2\u05D9\u05DD \u05D1\u05D4\u05D1\u05E0\u05D4\
  \ \u05D8\u05D5\u05D1\u05D4 \u05D9\u05D5\u05EA\u05E8 \u05E9\u05DC \u05D4\u05E6\u05E8\
  \u05DB\u05D9\u05DD\u2026"
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
---

{{< edit_this_page >}}

## מה ולמה?
בכל פעם שמתחילים פרויקט חדש ב-Elixir, מנצלים זאת כדי לבנות מבנה המארגן את הקוד ומאפשר תחזוקה טובה יותר. פרויקטים חדשים מסייעים בהבנה טובה יותר של הצרכים ובכתיבת קוד מידתי ויעיל.

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
