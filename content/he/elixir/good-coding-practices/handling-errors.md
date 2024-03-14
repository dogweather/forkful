---
date: 2024-01-26 00:52:42.751284-07:00
description: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05E4\u05D9\u05E8\u05D5\u05E9\u05D5 \u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\
  \u05D3 \u05E9\u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\
  \ \u05E2\u05DD \u05E1\u05D9\u05D8\u05D5\u05D0\u05E6\u05D9\u05D5\u05EA \u05DC\u05D0\
  \ \u05E6\u05E4\u05D5\u05D9\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05DE\
  \u05E0\u05D5\u05E2 \u05E7\u05E8\u05D9\u05E1\u05D5\u05EA \u05D5\u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7 \u05E9\u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05D5\u05EA \u05E9\u05DC\u05D4\u05DD \u05D9\u05DB\u05D5\u05DC\u05D5\u05EA \u05DC\
  \u05D4\u05EA\u05D0\u05D5\u05E9\u05E9 \u05D1\u05E6\u05D5\u05E8\u05D4\u2026"
lastmod: '2024-03-13T22:44:38.786090-06:00'
model: gpt-4-1106-preview
summary: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05E4\u05D9\u05E8\u05D5\u05E9\u05D5 \u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\
  \u05D3 \u05E9\u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\
  \ \u05E2\u05DD \u05E1\u05D9\u05D8\u05D5\u05D0\u05E6\u05D9\u05D5\u05EA \u05DC\u05D0\
  \ \u05E6\u05E4\u05D5\u05D9\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05DE\
  \u05E0\u05D5\u05E2 \u05E7\u05E8\u05D9\u05E1\u05D5\u05EA \u05D5\u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7 \u05E9\u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05D5\u05EA \u05E9\u05DC\u05D4\u05DD \u05D9\u05DB\u05D5\u05DC\u05D5\u05EA \u05DC\
  \u05D4\u05EA\u05D0\u05D5\u05E9\u05E9 \u05D1\u05E6\u05D5\u05E8\u05D4\u2026"
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

טיפול בשגיאות פירושו כתיבת קוד שיכול להתמודד עם סיטואציות לא צפויות. מתכנתים עושים זאת כדי למנוע קריסות וכדי להבטיח שהתוכניות שלהם יכולות להתאושש בצורה מכובדת כאשר חוק מרפי מתפרע.

## איך לעשות:

ב-Elixir, אנו לעיתים קרובות משתמשים בהתאמת דפוסים ובפקודת `case` כדי לטפל בתוצאות שונות, כולל שגיאות.

```elixir
defmodule Example do
  def divide(a, b) do
    case b do
      0 -> {:error, "Cannot divide by zero."}
      _ -> {:ok, a / b}
    end
  end
end

# חלוקה מוצלחת
{:ok, result} = Example.divide(10, 2)
IO.puts("10 / 2 הוא #{result}")

# ניסיון לחלק באפס
{:error, reason} = Example.divide(10, 0)
IO.puts("שגיאה: #{reason}")
```

דוגמת פלט:
```
10 / 2 הוא 5.0
שגיאה: Cannot divide by zero.
```

כאשר אתה מריץ קוד זה ב-Elixir, תקבל תוצאה של חלוקה מוצלחת או הודעת שגיאה, בהתאם לקלט שלך. לא יהיו קריסות כאן!

## עיון נוסף

בעבר, טיפול בשגיאות התבצע לעיתים קרובות על ידי בדיקת ערכי חזרה. עם שורשים פונקציונליים של Elixir, יש לנו התאמת דפוסים וטאפלים מתויגים, כמו `{:ok, value}` או `{:error, reason}`, שהם יותר אלגנטיים.

ישנם דרכים נוספות לטפל בשגיאות ב-Elixir:

- **`try` ו-`rescue` של Elixir**, שדומים ל-`try-catch` המסורתי בשפות אימפרטיביות אך נמצאים בשימוש פחות תכוף בשל עדיפות השימוש המפורש ב-Elixir.
- **מפקחים (Supervisors) ו-GenServers**, חלק ממסגרת OTP של Elixir, שהם יותר על אודות סבילות לתקלות. הם פוקדים על תהליך הקוד שלך, ערוכים להפעיל אותו מחדש אם משהו משתבש.

מבחינת היישום, Elixir מבוססת על אמינות של Erlang. היא מתייחסת לשגיאות כסוג נוסף של הודעה שיש לטפל בה עם כל טובת ההתאמה של דפוסים והיסודות הפונקציונליים.

## ראה גם

לקריאה נוספת על טיפול בשגיאות ב-Elixir, בדוק:

- המדריך הרשמי של Elixir על [טיפול בשגיאות](https://elixir-lang.org/getting-started/try-catch-and-rescue.html).
- למד יותר על [תהליכים ו-OTP](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).
- פורום Elixir תמיד הוא מקום טוב לשאול שאלות: [https://elixirforum.com](https://elixirforum.com).
