---
title:                "הדפסת פלט תיקון שגיאות"
html_title:           "Elixir: הדפסת פלט תיקון שגיאות"
simple_title:         "הדפסת פלט תיקון שגיאות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
הדפסת פלט דיבאג היא כלי רגיל בעולם התכנות שמשמש לבדיקת כיצד התוכנה רצה. המטרה העיקרית של שימוש בפלט דיבאג היא לזהות באילו נקודות הקוד יש תקלות או בעיות כדי להפעיל מנגנוני תיקון.

## איך לעשות?
תחת ```Elixir...``` יש להציג כמה דוגמאות לקוד עם הפלט המתאים בכדי להמחיש איך הדפסת פלט דיבאג עובדת

```Elixir
defmodule Example do
   def add(x, y) do
      IO.puts "The values being added are #{x} and #{y}."
      x + y
   end
end
```

```Elixir
iex> Example.add(3, 5)
The values being added are 3 and 5.
8
```

## חקירה מעמיקה
הדפסת פלט דיבאג הייתה תכונה רגילה ונפוצה בתכנות מחשבים משנות ה-70 של המאה ה-20. נכון להיום, ישנן טכניקות אחרות שמשמשות כחלופות לדיבאג, כגון השתמשות במשקלני מנוע ניפוח. כלומר, מנגנונים אוטומטיים המנתחים את הקוד כדי לזהות באילו נקודות מופיעים תקלות ובעיות.

## ראו גם
קישורים למקורות נוספים על דיבאג ב-Elixir 

- [Elixir - The Debugger's Tool](https://www.elixir-lang.org/getting-started/debugging.html)
- [Debugging in Elixir with IEx](https://semaphoreci.com/community/tutorials/debugging-phoenix-with-iex)
- [Elixir's built-in Debugging tools](https://hexdocs.pm/iex/IEx.Helpers.html)