---
date: 2024-01-20 17:52:42.624029-07:00
description: "\u05D3\u05D9\u05D1\u05D0\u05D2\u05D9\u05E0\u05D2 \u05D4\u05D5\u05D0\
  \ \u05DB\u05DC\u05D9 \u05E7\u05E8\u05D9\u05D8\u05D9 \u05D1\u05D0\u05E8\u05E1\u05E0\
  \u05DC \u05E9\u05DC \u05D4\u05DE\u05EA\u05DB\u05E0\u05EA. \u05D6\u05D4 \u05DE\u05D0\
  \u05E4\u05E9\u05E8 \u05DC\u05DA \u05DC\u05D4\u05D3\u05E4\u05D9\u05E1 \u05E4\u05DC\
  \u05D8 \u05E9\u05D9\u05DB\u05D5\u05DC \u05DC\u05E2\u05D6\u05D5\u05E8 \u05DC\u05DA\
  \ \u05DC\u05D0\u05EA\u05E8 \u05D1\u05D0\u05D2\u05D9\u05DD \u05D1\u05E7\u05D5\u05D3\
  \ \u05E9\u05DC\u05DA. \u05D0\u05E0\u05D5 \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\
  \ \u05D1\u05D6\u05D4 \u05DB\u05D9 \u05DC\u05E4\u05E2\u05DE\u05D9\u05DD \u05D4\u05DB\
  \u05D9 \u05E4\u05E9\u05D5\u05D8 \u05D4\u05D5\u05D0 \u05D4\u05DB\u05D9 \u05D8\u05D5\
  \u05D1: \u05E8\u05D5\u05D0\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.778004-06:00'
model: gpt-4-1106-preview
summary: "\u05D3\u05D9\u05D1\u05D0\u05D2\u05D9\u05E0\u05D2 \u05D4\u05D5\u05D0 \u05DB\
  \u05DC\u05D9 \u05E7\u05E8\u05D9\u05D8\u05D9 \u05D1\u05D0\u05E8\u05E1\u05E0\u05DC\
  \ \u05E9\u05DC \u05D4\u05DE\u05EA\u05DB\u05E0\u05EA."
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD"
weight: 33
---

## What & Why? (מה ולמה?)
דיבאגינג הוא כלי קריטי בארסנל של המתכנת. זה מאפשר לך להדפיס פלט שיכול לעזור לך לאתר באגים בקוד שלך. אנו משתמשים בזה כי לפעמים הכי פשוט הוא הכי טוב: רואים מה הקוד עושה צעד-אחר-צעד.

## How to (איך לעשות:)
ב-Elixir, אפשר להשתמש בפונקציה `IO.inspect` כדי להדפיס מידע לדיבאגינג:

```elixir
defmodule Example do
  def who_is_it?(name) do
    name 
    |> String.upcase()
    |> IO.inspect(label: "Name in uppercase")
  end
end

Example.who_is_it?("elixir")
```

פלט דוגמה:

```
Name in uppercase: "ELIXIR"
```

`IO.inspect` מחזירה את הערך שהיא מדפיסה, אז תוכל לשרשר אותה בתוך פייפליינים.

## Deep Dive (לעומק המים)
בעולמות התכנות העתיקים, כמו ב-C, היינו משתמשים ב-`printf` לדיבאגינג. ב-Elixir, `IO.inspect` הפך לכלי העדיף מאחר והוא מחזיר את הערך ותופס פחות מקום. במקום `IO.inspect`, יש גם את Logger, שמאפשר לוגינג מפורט יותר עם רמות שונות. בחירה בין השניים תלויה בצורך: `IO.inspect` לבדיקות מהירות, Logger למעקב אחר מערכת שלמה.

## See Also (ראו גם)
- [Elixir `IO` Module Documentation](https://hexdocs.pm/elixir/IO.html)
- [Elixir `Logger` Module Documentation](https://hexdocs.pm/logger/Logger.html)
- [Programming Elixir](https://pragprog.com/titles/elixir16/programming-elixir-1-6/) - ספר המכיל דיונים יסודיים על כלים לדיבאג ולניטור הרצת קוד.
- [The Pragmatic Programmer](https://pragprog.com/titles/tpp20/the-pragmatic-programmer-20th-anniversary-edition/) - מובאים שיטות וטכניקות כלליות לתכנות אפקטיבי ובטוח, כולל דיבאגינג.
