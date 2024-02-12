---
title:                "קבלת התאריך הנוכחי"
aliases:
- /he/elixir/getting-the-current-date/
date:                  2024-02-03T19:10:11.974939-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
  - 2024-02-05, dogweather, reviewed and corrected
lastmod:              2024-02-05
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי ב-Elixir כוללת גישה למידע על התאריך והשעה של המערכת, משימה נפוצה עבור תיעוד, חותמת נתונים, או כל פונקציונליות שדורשת ידע על התאריך הנוכחי. הפעולה הזו חיונית ליצירת יישומים חכמים לזמן ולמשימות כמו יצירת דוחות או חותמות זמן ביישום אינטרנטי.

## איך לעשות:
ספריית הסטנדרט של Elixir, דרך המודול `DateTime`, מאפשרת לאחזר את התאריך והשעה הנוכחיים. מאחר ש-Elixir רץ על JVM של Erlang (BEAM), הוא נכנס לתכונות הבסיסיות של Erlang לגבי פעולות של זמן.

### שימוש בספריית הסטנדרט של Elixir
Elixir מספקת את הפונקציה `DateTime.utc_now/0` כדי לקבל את התאריך והשעה הנוכחיים ב-UTC.

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**פלט לדוגמה:**
```
~U[2024-02-05 19:58:40.925931Z]
```

כדי לקבל רק את התאריך הנוכחי, תוכל לחלץ את רכיבי השנה, החודש, והיום:

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**פלט לדוגמה:**
```
~D[2023-05-04]
```

### שימוש בספריית Timex
עבור דרישות מורכבות יותר של תאריך-זמן, ניתן להשתמש בספרייה צד שלישי פופולרית בשם Timex. תחילה, הוסף את `Timex` לתלות ב-mix.exs שלך:

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

לאחר התקנת התלות (`mix deps.get`), אתה יכול להשתמש ב-Timex כדי לקבל את התאריך הנוכחי:

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**פלט לדוגמה:**
```
~D[2023-05-04]
```

Timex מציע פונקציות רבות למניפולציה של תאריך-זמן, הופכת אותה לתוספת חזקה ליישומי Elixir שלך במיוחד כאשר מתעסקים עם אזורי זמן, עיצוב ופירוק של תאריכים וזמנים.

על ידי הבנה ושימוש ביכולות הטבעיות של Elixir ובספריית Timex, אתה יכול בקלות לעבוד עם תאריכים ושעות ביישומי Elixir שלך, מותאם אישית לצרכים של היישום שלך בדייקנות ובנוחות.
