---
date: 2024-01-20 17:42:21.030061-07:00
description: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\
  \u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05EA\u05D1\u05E0\u05D9\u05EA \u05D4\u05D9\
  \u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D5 \u05DE\
  \u05E1\u05D9\u05E8\u05D9\u05DD \u05EA\u05D5\u05D5\u05D9\u05DD \u05DE\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05E2\u05DC \u05E4\u05D9 \u05DB\u05DC\u05DC \u05DE\u05E1\
  \u05D5\u05D9\u05DD. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D0\u05EA \u05D6\u05D4 \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05DB\
  \u05D3\u05D9 \u05DC\u05E0\u05E7\u05D5\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  , \u05DC\u05E4\u05E8\u05DE\u05D8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\
  \u05EA\u05E6\u05D5\u05E8\u05D4\u2026"
lastmod: '2024-03-11T00:14:12.168911-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05EA\u05D1\u05E0\u05D9\u05EA \u05D4\u05D9\u05D0\
  \ \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D5 \u05DE\u05E1\
  \u05D9\u05E8\u05D9\u05DD \u05EA\u05D5\u05D5\u05D9\u05DD \u05DE\u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05E2\u05DC \u05E4\u05D9 \u05DB\u05DC\u05DC \u05DE\u05E1\u05D5\
  \u05D9\u05DD. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D0\u05EA \u05D6\u05D4 \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05DB\u05D3\
  \u05D9 \u05DC\u05E0\u05E7\u05D5\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\
  \u05E4\u05E8\u05DE\u05D8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\u05EA\
  \u05E6\u05D5\u05E8\u05D4\u2026"
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים שתואמים תבנית היא פעולה שבה אנו מסירים תווים ממחרוזת על פי כלל מסוים. תכנתים עושים את זה בדרך כלל כדי לנקות נתונים, לפרמט מחרוזות לתצורה מסוימת, או להוריד מידע לא רלוונטי.

## איך לעשות:
```elixir
# תחילית פשוטה להסרת כל הספרות ממחרוזת.
String.replace("אני חי בשנת 2023", ~r/\d+/, "")
# Output: "אני חי בשנת "

# להסרת תווים מיוחדים להשאר רק עם אותיות ורווחים.
String.replace("היי! איך הולך? :)", ~r/[^א-ת ]/, "")
# Output: "היי איך הולך "

# הסרת כל רווחי התחילית והסוף.
String.trim("   זה בדיקה    ")
# Output: "זה בדיקה"
```

## צלילה עמוקה
מחיקת תווים תואמים לתבנית היא חלק מטיפול בביטויים רגולריים (Regular Expressions), שהחלו להופיע בשנות ה-50 של המאה ה-20. באליקסיר, אנו משתמשים במודול String ובפונקציות כמו `replace/3` ו-`trim/1` כדי לממש את המחיקה הזו בצורה קלה וגמישה. אלטרנטיבות כוללות תכנתים מובנים אחרים כמו `strip/1` ו-`slice/2` או שימוש במניפולציה ידנית של רשימת התווים באמצעות רקורסיה או איטראציה.

## ראה גם
- [Elixir's String Module](https://hexdocs.pm/elixir/String.html)
- [Regular Expressions in Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Programming Elixir ≥ 1.6 book by Dave Thomas](http://pragprog.com/book/elixir16/programming-elixir-1-6)
