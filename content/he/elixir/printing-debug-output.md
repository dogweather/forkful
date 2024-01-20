---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
הדפסת פלט לאיתור תקלות הוא דרך שבה מתכנתים מציגים מידע על תהליך הריצה של הקוד לבחינתו. הם אותרים בעיות תוך שימוש בכלים של שפת התכנות, כאמור, באמצעות הדפסות.

## איך להשתמש:
מספר דוגמאות איך להשתמש בכלים של Elixir להדפסת המידע:

```elixir
IO.puts "Hello, world!"
```

```elixir
defmodule MyModule do
  def my_function do
    IO.puts "This is a debug message"
    # rest of the code...
  end
end
```

## צולעים עמוק:
פלט ה debug הוא כלי שנמצא בשימוש מאז ימי המחשב הראשונים. זה הדרך האינטואטיבית ביותר להבין את הדאטה שהמחשב טופל. אבל מהדפסת פלט debug אינה הדרך היעילה ביותר לאתר בעיות, נמתחים עכשיו מערכות לפרנסיסקה שנראים כמו Graphical Debuggers. ב Elixir, מודול IO מתקשר עם ה kernel של erlang כדי להדפיס מידע.

## ראה גם:
1. [דוגמאות IO של Elixir](https://hexdocs.pm/elixir/IO.html)
2. [מדריך תיקון שגיאות אליקסיר](https://elixirschool.com/en/lessons/specifics/debugging/)
3. [עיונים במערכת erlang](http://erlang.org/doc/apps/erts/io_protocol.html)