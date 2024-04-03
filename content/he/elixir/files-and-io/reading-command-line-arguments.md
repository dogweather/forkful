---
date: 2024-01-20 17:55:44.496655-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:38.799446-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
weight: 23
---

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
