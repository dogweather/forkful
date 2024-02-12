---
title:                "טיפול בשגיאות"
aliases: - /he/elixir/handling-errors.md
date:                  2024-01-26T00:52:42.751284-07:00
model:                 gpt-4-1106-preview
simple_title:         "טיפול בשגיאות"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/handling-errors.md"
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
