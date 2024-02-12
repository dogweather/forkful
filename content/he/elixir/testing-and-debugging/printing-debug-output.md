---
title:                "הדפסת פלט לניפוי באגים"
aliases:
- /he/elixir/printing-debug-output.md
date:                  2024-01-20T17:52:42.624029-07:00
model:                 gpt-4-1106-preview
simple_title:         "הדפסת פלט לניפוי באגים"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

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
