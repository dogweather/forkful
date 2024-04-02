---
date: 2024-01-20 17:45:48.698045-07:00
description: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05DE\u05D3\u05D1\u05E8 \u05E2\u05DC \u05DC\u05E7\u05D9\u05D7\
  \u05EA \u05D7\u05DC\u05E7 \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E7\u05D9\
  \u05D9\u05DE\u05EA. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\
  \u05D1\u05D3 \u05D0\u05D5 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05E1\u05E4\u05E6\u05D9\u05E4\u05D9\u05D9\u05DD \u05D1\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA."
lastmod: '2024-03-13T22:44:38.755296-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05DE\u05D3\u05D1\u05E8 \u05E2\u05DC \u05DC\u05E7\u05D9\u05D7\u05EA\
  \ \u05D7\u05DC\u05E7 \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E7\u05D9\u05D9\
  \u05DE\u05EA. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\
  \u05D3 \u05D0\u05D5 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05E1\u05E4\u05E6\u05D9\u05E4\u05D9\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
weight: 6
---

## מה ולמה?
חילוץ תת-מחרוזות מדבר על לקיחת חלק ממחרוזת קיימת. תוכניתנים עושים את זה כדי לעבד או לבדוק נתונים ספציפיים במחרוזת.

## איך לעשות:
קוד באליקסיר לחילוץ תת-מחרוזות:

```elixir
str = "שלום, עולם של אליקסיר!" 

# חילוץ באמצעות טווחים
substring = String.slice(str, 7, 5)
IO.puts substring
# Output: "עולם"

# חילוץ באמצעות ראשית ואורך
substring = String.slice(str, -6, 6)
IO.puts substring
# Output: "אליקסיר"
```

## עיון מעמיק:
בהיסטוריה, חילוץ תת-מחרוזות היה תמיד חלק מהתוכנות - כמעט כל שפה תומכת בזה. באליקסיר, יש כמה דרכים לעשות את זה, הדוגמאות מראות שימוש בפונקציה `String.slice/3`. דרך נוספת היא להשתמש ב-`binary pattern matching` אשר מציע גמישות רבה אבל דורש ידע מסוים בפטרנים. כאשר מבצעים חילוץ, חשוב לזכור שאליקסיר עובדת עם UTF-8 באופן אוטומטי, מה שאומר שתווים כמו "ש" או "ל" מחשבים כתווים בודדים, למרות שבמערכות אחרות הם עשויים לתפוס יותר מבית אחד.

## ראה גם:
- [Elixir String module documentation](https://hexdocs.pm/elixir/String.html)
- [Elixir School: Strings](https://elixirschool.com/en/lessons/basics/strings/)
- [Elixir Forum for questions and discussions](https://elixirforum.com/)
