---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 02:05:48.977230-07:00
description: "\u05DC\u05E0\u05EA\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05D0\u05D5\u05DE\u05E8 \u05DC\u05E7\
  \u05D7\u05EA \u05D8\u05E7\u05E1\u05D8, \u05DB\u05DE\u05D5 \"2023-04-05\", \u05D5\
  \u05DC\u05D4\u05DE\u05D9\u05E8\u05D5 \u05DC\u05E4\u05D5\u05E8\u05DE\u05D8 \u05EA\
  \u05D0\u05E8\u05D9\u05DA \u05E9\u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05E9\
  \u05DC\u05DA \u05D9\u05DB\u05D5\u05DC\u05D4 \u05DC\u05D4\u05D1\u05D9\u05DF \u05D5\
  \u05DC\u05E2\u05D1\u05D5\u05D3 \u05D0\u05D9\u05EA\u05D5. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DE\u05DB\
  \u05D9\u05D5\u05D5\u05DF \u05E9\u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.789542-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05E0\u05EA\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05D0\u05D5\u05DE\u05E8 \u05DC\u05E7\u05D7\
  \u05EA \u05D8\u05E7\u05E1\u05D8, \u05DB\u05DE\u05D5 \"2023-04-05\", \u05D5\u05DC\
  \u05D4\u05DE\u05D9\u05E8\u05D5 \u05DC\u05E4\u05D5\u05E8\u05DE\u05D8 \u05EA\u05D0\
  \u05E8\u05D9\u05DA \u05E9\u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05E9\u05DC\
  \u05DA \u05D9\u05DB\u05D5\u05DC\u05D4 \u05DC\u05D4\u05D1\u05D9\u05DF \u05D5\u05DC\
  \u05E2\u05D1\u05D5\u05D3 \u05D0\u05D9\u05EA\u05D5. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DE\u05DB\u05D9\
  \u05D5\u05D5\u05DF \u05E9\u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

לנתח תאריך ממחרוזת זה אומר לקחת טקסט, כמו "2023-04-05", ולהמירו לפורמט תאריך שהתוכנית שלך יכולה להבין ולעבוד איתו. מתכנתים עושים זאת מכיוון שתאריכים מגיעים בשפע של פורמטים, והם זקוקים לעקביות כדי להשוות, למיין או לאחסן אותם כהלכה.

## איך לעשות:

ב-Elixir, ניתן לנתח תאריכים באמצעות המודול `Date`. הנה איך להפוך מחרוזת לתאריך:

```elixir
date_string = "2023-04-05"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)
```

דוגמה לפלט:

```elixir
~D[2023-04-05]
```

כדי להתמודד עם פורמטים שונים, ניתן להשתמש בספריית `Timex`:

```elixir
{:ok, datetime} = Timex.parse("05-04-2023", "{D}-{0M}-{YYYY}")
IO.inspect(datetime)
```

דוגמה לפלט:

```elixir
#DateTime<2023-04-05 00:00:00Z>
```

## צלילה עמוקה

הפונקציה `Date.from_iso8601/1` היא חלק מספריית התקנים של Elixir, שהוצגה כדי להבטיח ניתוח פשוט של תקן התאריך ISO8601 - פורמט תאריך נפוץ. אך החיים לא כל כך פשוטים; תאריכים מגיעים בטונות של פורמטים. זה המקום שבו `Timex`, ספרייה צד שלישי של Elixir, מתמקמת. היא עשירה יותר מפונקציות התאריך המובנות של Elixir ועוזרת להתמודד עם מגוון רחב של פורמטי תאריך.

Elixir עצמה היא בלתי משתנה, מה שאומר שתאריכים שננתחו אינם יכולים להשתנות לאחר יצירתם. תכונה זו חוזרת לשורשי התכנות הפונקציונאלי של Elixir, מבטיחה צפיות וניפוי באגים קל יותר.

מבחינה היסטורית, ניתוח תאריכים היה משימה קשה בשל התקנים משתנים. עם זאת, בעזרת ספריות כמו `Timex` ותכונות שפה ב-Elixir, המורכבות הזו מוסתרת, מה שהופך את חיי המפתחים לפשוטים יותר.

## ראה גם

- [Elixir Date](https://hexdocs.pm/elixir/Date.html)
- [תיעוד Timex](https://hexdocs.pm/timex/Timex.html)
- [התקן ISO8601](https://www.iso.org/iso-8601-date-and-time-format.html)
