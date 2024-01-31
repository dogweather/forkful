---
title:                "פיענוח תאריך ממחרוזת"
date:                  2024-01-28T02:05:48.977230-07:00
model:                 gpt-4-0125-preview
simple_title:         "פיענוח תאריך ממחרוזת"

category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
