---
title:                "כתיבת בדיקות"
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
כתיבת בדיקות היא תהליך בו כותבים סקריפטים שמריצים קטעי קוד כדי לוודא שהם עובדים כראוי. מתכנתים עושים את זה כי זה מבטיח יציבות ומזהה באגים לפני השקה לייצור.

## איך לעשות:
ב-Elixir נעשה שימוש ב-`ExUnit` לכתיבת טסטים. הנה דוגמה פשוטה:

```elixir
defmodule ExampleTest do
  use ExUnit.Case
  doctest Example

  test "the truth" do
    assert 1 + 1 == 2
  end
end
```

הפקת תוצאות:
```plaintext
..

Finished in 0.03 seconds
1 tests, 0 failures

Randomized with seed 123456
```

## Deep Dive:
כתיבת טסטים התפתחה עם השנים לכדי תפיסת עולם מלאה בעולם התוכנה. ב-Elixir, מבוסס על פילוסופיית 'טסט מופעל קוד', `ExUnit` הוא הפריימוורק המוביל לטסטים שמגיע עם השפה. קיימות אלטרנטיבות כמו `Espec`, המציעה סגנון תיאורי יותר, אך `ExUnit` נשאר הפופולרי עקב הפשטות והקלות של שימוש. מימוש הטסטים מתבצע בדרך כלל במהלך הפיתוח ולא לאחריו, לכל השינויים וההוספות בקוד.

## ראה גם:
- [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School - Testing](https://elixirschool.com/en/lessons/basics/testing/)
- [Elixir Testing Guidelines](https://elixir-lang.org/getting-started/mix-otp/docs-tests-and-with.html)
