---
title:    "Elixir: כתיבת מבחנים"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## מדוע

ניסיון בכתיבת בדיקות הוא אחד הכלים המרכזיים בתהליך כתיבת קוד יעיל ואיכותי ב-Elixir. בדיקות מאפשרות לנו לוודא שהקוד שאנחנו כותבים עובד בדיוק כפי שאנו מצפים, ומסייעות לנו למצוא ולתקן באופן מהיר אם יש באגים בקוד.

## כיצד לכתוב בדיקות ב-Elixir

לכתיבת בדיקות ב-Elixir יש ערך רב ויש מספר דרכים לעשות זאת. בהמשך נשתמש בקודים דוגמה ופלטות כדי להראות כיצד ניתן לבדוק קוד ב-Elixir.

לדוגמה, נכתוב פונקציה פשוטה המחשבת סכום של שני מספרים ונבדוק את הפלט שלה:

```Elixir
defmodule Calculator do
  def add(a, b) do
    a + b
  end
end

IO.puts Calculator.add(2, 3)

# Output: 5
```

כעת ניצור מקרה קצה שבו ניתן יהיה לגרום לפונקציה שלנו להתקלקל ונצטרך להבטיח שהיא עדיין פועלת כראוי:

```Elixir
defmodule CalculatorTest do
  use ExUnit.Case

  test "adding two numbers" do
    assert Calculator.add(2, 3) == 5
  end

  test "adding a string and a number" do
    assert Calculator.add("2", 3) == 5
  end
end
```

הרצת הבדיקות על ידי הרצת ```mix test``` תחזיר פלט שנראה כך:

```
iex> mix test

..

Finished in 0.07 seconds
2 tests, 0 failures
```

צמצמת זה הדוגמה המובנית, אך ניתן לבדוק כמה מקרים גם בקודים גדולים ומורכבים רבים יותר.

## מעמקי הבדיקות

כדי להפוך למומחה בכתיבת בדיקות ב-Elixir, חשוב להבין גם מעמקי הבדיקות. הבדיקות ב-Elixir מתבצעות באמצעות המודול ExUnit, המציג פונקציות מובנות כמו ```assert``` ו-```refute```, שמאפשרות לנו לבדוק מצבים שונים כדי לוודא שה