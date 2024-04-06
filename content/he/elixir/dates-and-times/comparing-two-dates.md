---
date: 2024-01-20 17:32:39.680975-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E4\u05E2\u05DD\
  , \u05D3\u05D1\u05E8 \u05DB\u05D6\u05D4 \u05D4\u05D9\u05D4 \u05E1\u05D9\u05D5\u05D8\
  \ \u05E9\u05DC \u05D4\u05E9\u05D5\u05D5\u05D0\u05D5\u05EA \u05D9\u05D3\u05E0\u05D9\
  \u05D5\u05EA \u05E9\u05DC \u05E9\u05E0\u05D4, \u05D7\u05D5\u05D3\u05E9 \u05D5\u05D9\
  \u05D5\u05DD. \u05D4\u05D9\u05D5\u05DD \u05D9\u05E9 \u05DC\u05E0\u05D5 \u05DE\u05D5\
  \u05D3\u05D5\u05DC\u05D9\u05DD \u05DB\u05DE\u05D5 Timex \u05D1-Elixir \u05E9\u05DE\
  \u05E4\u05E9\u05D8\u05D9\u05DD \u05D0\u05EA \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA\
  . \u05D9\u05E9\u05E0\u05DD \u05D0\u05DC\u05D8\u05E8\u05E0\u05D8\u05D9\u05D1\u05D5\
  \u05EA \u05E0\u05D5\u05E1\u05E4\u05D5\u05EA \u05DB\u05DE\u05D5\u2026"
lastmod: '2024-04-05T21:53:40.085482-06:00'
model: gpt-4-1106-preview
summary: "\u05E4\u05E2\u05DD, \u05D3\u05D1\u05E8 \u05DB\u05D6\u05D4 \u05D4\u05D9\u05D4\
  \ \u05E1\u05D9\u05D5\u05D8 \u05E9\u05DC \u05D4\u05E9\u05D5\u05D5\u05D0\u05D5\u05EA\
  \ \u05D9\u05D3\u05E0\u05D9\u05D5\u05EA \u05E9\u05DC \u05E9\u05E0\u05D4, \u05D7\u05D5\
  \u05D3\u05E9 \u05D5\u05D9\u05D5\u05DD."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05E0\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
weight: 27
---

## איך לעשות:
```elixir
# התקנת תלות עם הפקודה:
# mix deps.get {:timex, "~> 3.7"}

# ייבוא תלות
alias Timex

# יצירת שני תאריכים
date1 = ~D[2023-04-01]
date2 = ~D[2023-04-02]

# שוויון תאריכים ע"י Timex
Timex.compare(date1, date2) # יחזיר -1 אם date1 לפני date2
Timex.compare(date1, date1) # יחזיר 0 אם התאריכים זהים
Timex.compare(date2, date1) # יחזיר 1 אם date2 אחרי date1
```

## הצלילה לתוך הנושא:
פעם, דבר כזה היה סיוט של השוואות ידניות של שנה, חודש ויום. היום יש לנו מודולים כמו Timex ב-Elixir שמפשטים את התהליך. ישנם אלטרנטיבות נוספות כמו Elixir's Date module אבל Timex מציע יותר פעולות ויכולות פורמט. לגבי פרטי מימוש, Timex משתמש באיזון בין נוחות לביצועים ע"י שימוש במידע של זמנים מהמחשב וסיבוכיות של חישובי תאריכים.

## ראה גם:
- [Timex Documentation](https://hexdocs.pm/timex/Timex.html)
- [Elixir Date module](https://hexdocs.pm/elixir/Date.html)
- פוסט בפורום [Elixir Forum](https://elixirforum.com) על שוויון תאריכים
- כתבה על [יחסים בין תאריכים](https://date-fns.org/v2.21.3/docs/compareAsc) בספריית ה-JavaScript date-fns
