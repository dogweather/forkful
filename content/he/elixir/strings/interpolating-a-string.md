---
date: 2024-01-20 17:51:06.661553-07:00
description: "\u05DE\u05D9\u05DC\u05D5\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05D4\u05D5\u05D0 \u05E9\u05D9\u05D8\u05D4 \u05DC\u05D4\u05D6\u05E8\u05D9\u05E7\
  \ \u05EA\u05D5\u05DB\u05DF \u05DE\u05EA\u05D5\u05DA \u05DE\u05E9\u05EA\u05E0\u05D9\
  \u05DD \u05D0\u05D5 \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05DC\u05EA\u05D5\
  \u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E7\u05D9\u05D9\u05DE\u05EA. \u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\
  \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D1\u05E0\u05D5\u05EA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA \u05D1\u05D0\u05D5\u05E4\u05DF \u05D3\u05D9\u05E0\u05DE\
  \u05D9 \u05D5\u05DC\u05E9\u05DC\u05D1 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\
  \u05E7\u05DC\u05D5\u05EA \u05EA\u05D5\u05DA \u05DB\u05D3\u05D9\u2026"
lastmod: '2024-03-13T22:44:38.750657-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D9\u05DC\u05D5\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\
  \u05D5\u05D0 \u05E9\u05D9\u05D8\u05D4 \u05DC\u05D4\u05D6\u05E8\u05D9\u05E7 \u05EA\
  \u05D5\u05DB\u05DF \u05DE\u05EA\u05D5\u05DA \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD\
  \ \u05D0\u05D5 \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05DC\u05EA\u05D5\u05DA\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E7\u05D9\u05D9\u05DE\u05EA."
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 8
---

## איך לעשות:
באליקסיר, תשתמשו בסימני `#{}` כדי לבצע מילוי מחרוזת. דוגמאות:

```elixir
name = "אורי"
age = 32

# מילוי בסיסי
greeting = "שלום, שמי הוא #{name} ואני בן #{age}."
IO.puts greeting
# פלט: שלום, שמי הוא אורי ואני בן 32.

# מילוי עם ביטוי
info = "עוד שנה אהיה בן #{age + 1}."
IO.puts info
# פלט: עוד שנה אהיה בן 33.
```

## צלילה לעומק
מילוי מחרוזת הוא לא המצאה חדשה וכבר קיימת בשפות כמו Ruby או Python. באליקסיר, התכנית מבצעת באופן אוטומטי את הפעולה בזמן הרצה של הקוד. המטפל בזה הוא ה-VM של Erlang, שעליו בנויה אליקסיר. פרטים נוספים: המילוי הוא חלק מה-String interpolation, והוא משתמש במודול `String` שמקבל תמיכה עמוקה בעבודה עם טקסט. אלטרנטיבה ישנה הייתה להדביק מחרוזות על ידי סימני חיבור (`++`), אבל זה פחות אלגנטי ויעיל.

## ראו גם:
- איך עובד `String` מודול באליקסיר: [Elixir String module](https://hexdocs.pm/elixir/String.html)
- מדריך למילוי מחרוזת באליקסיר: [Elixir Interpolation Guide](https://elixirschool.com/en/lessons/basics/strings/#interpolation)
- דוקומנטציה רשמית של אליקסיר: [Official Elixir documentation](https://elixir-lang.org/docs.html)
