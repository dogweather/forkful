---
title:                "שרבוב מחרוזת"
aliases:
- /he/elixir/interpolating-a-string/
date:                  2024-01-20T17:51:06.661553-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מילוי מחרוזת הוא שיטה להזריק תוכן מתוך משתנים או ביטויים לתוך מחרוזת קיימת. תכנתים משתמשים בזה כדי לבנות מחרוזות באופן דינמי ולשלב נתונים בקלות תוך כדי קוד.

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
