---
title:                "חילוץ תת-מחרוזות"
date:                  2024-01-20T17:45:48.698045-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

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
