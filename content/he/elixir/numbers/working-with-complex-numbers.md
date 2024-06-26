---
date: 2024-01-26 04:39:39.675578-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Elixir \u05D0\
  \u05D9\u05DF \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\
  \u05D9\u05DD \u05DE\u05D5\u05D1\u05E0\u05D9\u05DD, \u05D0\u05D6 \u05D0\u05E0\u05D7\
  \u05E0\u05D5 \u05D9\u05D5\u05E6\u05E8\u05D9\u05DD \u05DE\u05E9\u05DC\u05E0\u05D5\
  \ \u05D0\u05D5 \u05E9\u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05D4, \u05DB\u05DE\u05D5 `ComplexNum`. \u05D4\u05E0\u05D4 \u05D3\
  \u05D5\u05D2\u05DE\u05D4 \u05DE\u05D4\u05D9\u05E8\u05D4 \u05E2\u05DD \u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05D4."
lastmod: '2024-03-13T22:44:38.763433-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Elixir \u05D0\u05D9\u05DF \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\
  \u05E8\u05D5\u05DB\u05D1\u05D9\u05DD \u05DE\u05D5\u05D1\u05E0\u05D9\u05DD, \u05D0\
  \u05D6 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05D9\u05D5\u05E6\u05E8\u05D9\u05DD \u05DE\
  \u05E9\u05DC\u05E0\u05D5 \u05D0\u05D5 \u05E9\u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\
  \u05DD \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4, \u05DB\u05DE\u05D5 `ComplexNum`."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

## איך לעשות:
ב-Elixir אין מספרים מרוכבים מובנים, אז אנחנו יוצרים משלנו או שמשתמשים בספרייה, כמו `ComplexNum`. הנה דוגמה מהירה עם ספרייה:

```elixir
# בהנחה שהתקנתם את ComplexNum
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# צור מספרים מרוכבים וחבר אותם
c1 = {3, 4}   # מייצג 3 + 4i
c2 = {2, -3}  # מייצג 2 - 3i
result = ComplexMath.add(c1, c2)
IO.puts "התוצאה היא: #{inspect(result)}"
```

זה יפיק:
```
התוצאה היא: {5, 1}
```

זה אומר שסכום של `3 + 4i` ו-`2 - 3i` הוא `5 + 1i`.

## נטילה לעומק
מספרים מרוכבים צצו בהיסטוריה מכיוון שמספרים רגילים לא הסתדרו עם שורשים ריבועיים של מספרים שליליים. רק במאה ה-17 הם זכו להיחשבות ברצינות, הודות למתמטיקאים כמו רנה דקארט וג'רולאמו קרדאנו.

ב-Elixir, לעיתים קרובות משתמשים בטאפלים כמו `{3, 4}` עבור מספרים מרוכבים, או בספרייה מוקדשת כדי לא לחזור על גלגל. ספריות הן בדרך כלל טובות יותר - הן מטפלות בדברים הקטנים כמו כפל וחילוק, שהופכים מסובכים בגלל היחידה המדומה 'i' (לידיעתכם: `i` בריבוע שווה ל-`-1`).

## ראה גם
בדוק את המשאבים הבאים:
- [ספריית ComplexNum](https://hex.pm/packages/complex_num) עבור מנהל החבילות של Elixir, Hex.
- [בית ספר ל-Elixir](https://elixirschool.com/en/), לנושאים מתקדמים ותרגילים ב-Elixir.
- [Erlang -- המודול math](http://erlang.org/doc/man/math.html), אותו Elixir משתמשת בו מאחורי הקלעים, לצרכים מתמטיים אחרים.
