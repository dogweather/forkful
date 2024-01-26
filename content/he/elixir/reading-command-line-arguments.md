---
title:                "קריאת פרמטרים משורת הפקודה"
date:                  2024-01-20T17:55:44.496655-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים מקו הפקודה היא הדרך שבה תוכנית קולטת קלט מהמשתמש בעת הפעלתה. תכניתאים עושים זאת כדי להתאים את התוכנית לצרכים ספציפיים בכל הפעלה, בלי לשנות את הקוד עצמו.

## איך לעשות:
```Elixir
# קובץ main.exs
defmodule CLIApp do
  def main(args) do
    IO.inspect(args)
  end
end

# בשורת הפקודה:
elixir main.exs arg1 arg2 arg3
# פלט
["arg1", "arg2", "arg3"]
```

## צלילה לעומק
בעבר, שפות כמו C השתמשו ב`argc` ו`argv` כדי לקרוא ארגומנטים. Elixir, שמגיעה עם OTP (Open Telecom Platform), מבצעת את זה באופן שונה. הפונקציה `System.argv/0` מחזירה רשימה של ארגומנטים. אפשר גם לשנות את הארגומנטים לאחר הפעלת התוכנית באמצעות `System.argv/1`. בנוסף, בעזרת ספריות חיצוניות כמו `OptionParser`, אפשר לנתח פלגים ולהקל על פרסור ארגומנטים מורכבים יותר.

## ראה גם
- [מסמכי המשתמש של Elixir לקריאת ארגומנטים](https://hexdocs.pm/elixir/System.html#argv/0)
- [דוקומנטציה של OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
- [מדריך למתחילים ב Elixir](https://elixir-lang.org/getting-started/introduction.html)
