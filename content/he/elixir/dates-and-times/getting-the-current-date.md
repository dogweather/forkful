---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:10:11.974939-07:00
description: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\
  \u05E0\u05D5\u05DB\u05D7\u05D9 \u05D1-Elixir \u05DB\u05D5\u05DC\u05DC\u05EA \u05D2\
  \u05D9\u05E9\u05D4 \u05DC\u05DE\u05D9\u05D3\u05E2 \u05E2\u05DC \u05D4\u05EA\u05D0\
  \u05E8\u05D9\u05DA \u05D5\u05D4\u05E9\u05E2\u05D4 \u05E9\u05DC \u05D4\u05DE\u05E2\
  \u05E8\u05DB\u05EA, \u05DE\u05E9\u05D9\u05DE\u05D4 \u05E0\u05E4\u05D5\u05E6\u05D4\
  \ \u05E2\u05D1\u05D5\u05E8 \u05EA\u05D9\u05E2\u05D5\u05D3, \u05D7\u05D5\u05EA\u05DE\
  \u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D0\u05D5 \u05DB\u05DC \u05E4\u05D5\
  \u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05E9\u05D3\u05D5\u05E8\
  \u05E9\u05EA \u05D9\u05D3\u05E2 \u05E2\u05DC \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA\
  \u2026"
lastmod: '2024-03-13T22:44:38.791300-06:00'
model: gpt-4-0125-preview
summary: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9 \u05D1-Elixir \u05DB\u05D5\u05DC\u05DC\u05EA \u05D2\u05D9\
  \u05E9\u05D4 \u05DC\u05DE\u05D9\u05D3\u05E2 \u05E2\u05DC \u05D4\u05EA\u05D0\u05E8\
  \u05D9\u05DA \u05D5\u05D4\u05E9\u05E2\u05D4 \u05E9\u05DC \u05D4\u05DE\u05E2\u05E8\
  \u05DB\u05EA, \u05DE\u05E9\u05D9\u05DE\u05D4 \u05E0\u05E4\u05D5\u05E6\u05D4 \u05E2\
  \u05D1\u05D5\u05E8 \u05EA\u05D9\u05E2\u05D5\u05D3, \u05D7\u05D5\u05EA\u05DE\u05EA\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D0\u05D5 \u05DB\u05DC \u05E4\u05D5\u05E0\
  \u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05E9\u05D3\u05D5\u05E8\u05E9\
  \u05EA \u05D9\u05D3\u05E2 \u05E2\u05DC \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA\u2026"
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

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
