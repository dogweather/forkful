---
title:                "השוואת שני תאריכים"
aliases:
- /he/elixir/comparing-two-dates.md
date:                  2024-01-20T17:32:39.680975-07:00
model:                 gpt-4-1106-preview
simple_title:         "השוואת שני תאריכים"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
שוויון תאריכים משמעותו בדיקה אם שני ערכי תאריך תואמים זה לזה, ומתי זה קורה זה חיוני לפיצ'רים כמו הזמנות, משימות מתוזמנות וגיבויי נתונים.

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
