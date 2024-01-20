---
title:                "התחלת פרויקט חדש"
html_title:           "Clojure: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?
פתיחת פרויקט חדש בתכנות הינה תהליך שבו מתחילים לכתוב מקוד מאפס ומגדירים את מבנה הפרויקט. מתכנתים בוחרים לפתוח פרויקט חדש כדי ליישם רעיונות חדשים, לבנות פתרונות מותאמים אישית או פשוט בשביל הכיף והלמידה.

## איך לעשות:
איך אנחנו מתחילים פרויקט חדש ב-Elixir? מתחילים כך:

```elixir
mix new my_project
cd my_project
```

בקרוב, תראה מבנה פרויקט בסיסי כמו הבא:

```elixir
my_project/
  ├── lib/
  │   └── my_project.ex
  ├── test/
  │   └── my_project_test.exs
  ├── mix.exs
  └── README.md
```

לוודא שהכל מוכן, אפשר להריץ את הפקודה הבאה:

```elixir
mix test
```

## Deep Dive
Elixir היא שפת תכנות פונקציונלית, אינטראקטיבית, מבוססת על שפת Erlang. היא הושקה בשנת 2011 על ידי José Valim, היוצר העיקרי של השפה. למרות שוודאי ישנן שפות תכנות אחרות לבניית פרויקטים חדשים, Elixir היא אפשרות טובה בזכות הפונקציונליות שלה ומערך הכלים העשיר שלה, כולל `mix`, לניהול תלויות ו- build.

החל מעקבים של "Hello, World!" וכל הדרך לתכנית המורכבת ביותר, יש ל Elixir המדריך למשתמש לארגז הכלים Mix תוך כדי הסבר כיצד להגדיר, לנהל ולהריץ פרויקטים של Elixir.

## ראה גם
1. [Elixir's Starting Page](https://elixir-lang.org/getting-started/introduction.html)
2. [Elixir's Mix help](https://hexdocs.pm/mix/Mix.html)
3. [Elixir School](https://elixirschool.com/)