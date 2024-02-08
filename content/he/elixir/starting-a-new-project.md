---
title:                "התחלת פרויקט חדש"
aliases:
- he/elixir/starting-a-new-project.md
date:                  2024-01-20T18:04:00.889055-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/starting-a-new-project.md"
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
