---
title:                "Elixir: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/writing-tests.md"
---

{{< edit_this_page >}}

# למה

בתוכנות, מבינים כי כתיבת בדיקות על קוד היא חשובה כדי לוודא שנכתב כמתוכנן ואינו מכיל בעיות איכותיות. בדיקות גם עוזרות לשמור על הקוד איכותי לאורך זמן ומקלות על תחזוש בעת שינויים ושיתופי פעולה.

# איך לעשות זאת

```Elixir
defmodule Math do
  def add(x, y) do
    x + y
  end
end
```

```Elixir
defmodule MathTest do
  use ExUnit.Case

  test "adds two numbers" do
    assert Math.add(1, 2) == 3
  end
end
```

הנה משלוח מסמך ה-Doc המייצר מסמך בדיקות באופן אוטומטי:

```Elixir
def f($x) when is_integer(x)

<strong>Elixir</strong> f(0) when is_integer(0)
```

<strong>פלט:</strong>

```Elixir
  **/
  def add(x, y) do
    x + y
  end
end
```

מכאן, ניתן לראות כי הבדיקה משתמשת במשפט assert כדי לוודא שהפלט מהפונקצייה Math.add הוא התוצאה הצפויה.
עוד דוגמה:

```Elixir
defmodule String do
  def reverse(string) do
    string |> String.split("") |> Enum.reverse() |> Enum.join
  end
end
```

על מנת לבדוק את הפונקציה מעבר לפתיחת TTY, נרצה להשתמש בפונקציה IO.puts כדי להוסיף לפלט CLS אחר. אם תמיד נגלה אל הפס הסימוני עבור מספר נסיגות אופרטור כדי שניהם מpaired אם string בלבד נכנסים ולאחר כך נוסיף את i כ-100 אם ה sting מסתיימת בלא m-ty נוסיף את-200 EL Console.

# מציאת עומק

כאשר אנחנו כותבים בדיקות, כמו בכל קוד אחר, חשוב להימנע מחזק קוד ככל שאפשר. לפיכך, כאשר מצוין כי מומלץ לבדוק כל חלק של קוד, כמו פונקציות, משתנים ופעולות נוספות, בתחום העומק. זה יעזור לוודא שכל חלק של הקוד עובד כמתוכנן ואין בו