---
date: 2024-01-20 17:38:11.247241-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\
  \u05D8\u05E0\u05D5\u05EA \u05D5\u05DC\u05DE\u05D4 \u05D6\u05D4 \u05E0\u05D7\u05D5\
  \u05E5? \u05DC\u05D4\u05DE\u05D9\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D6\u05D4\
  \ \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA \u05DB\u05DC \u05D4\u05D0\u05D5\u05EA\
  \u05D9\u05D5\u05EA \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05E4\u05D5\
  \u05E8\u05DE\u05D8 \u05D4\u05E7\u05D8\u05DF \u05E9\u05DC\u05D4\u05DF. \u05EA\u05D5\
  \u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DC\u05E6\u05D5\u05E8\u05DA\u2026"
lastmod: '2024-03-13T22:44:38.752243-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\
  \u05E0\u05D5\u05EA \u05D5\u05DC\u05DE\u05D4 \u05D6\u05D4 \u05E0\u05D7\u05D5\u05E5\
  ."
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

## How to:
ב-Elixir, משתמשים בפונקציה `String.downcase/1` כדי להמיר מחרוזת לאותיות קטנות:

```elixir
iex> my_string = "Shalom, OLAM!"
iex> String.downcase(my_string)
"shalom, olam!"
```

קל ופשוט. השתמש ב`String.downcase/1` וקיבלת את התוצאה.

## Deep Dive
ההמרה לאותיות קטנות היא לא פשוטה כמו שזה נשמע. יש לשקול תרבויות ושפות שונות עם סט של תווים מיוחדים. במקרה של Elixir, שהיא שפה תומכת UTF-8, היא מתמודדת עם המרות תווים לא אסקי גם יחד.

לפני שהייתה הפונקציה `String.downcase/1`, תוכניתנים היו צריכים להמיר את המחרוזות באופן ידני. זה היה תהליך מסורבל ופגיע לשגיאות.

כיום, הפונקציה `String.downcase/1` משתמשת בספריית Elixir עצמה או בספריות מערכת ההפעלה כדי לבצע המרות מורכבות של תווים בצורה יעילה ונכונה.

## See Also
- [Elixir String module](https://hexdocs.pm/elixir/String.html)
- [Unicode standards](https://unicode.org/standard/standard.html)
- [Elixir School: Strings](https://elixirschool.com/en/lessons/basics/strings/)
