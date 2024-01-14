---
title:                "Gleam: חישוב תאריך בעתיד או בעבר"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

כשמדובר בחישוב תאריך בעתיד או בעבר, יכול להיות שימושי לעשות זאת בכדי לתכנן אירוע או לקבל תאריך מדויק של אירוע שכבר קרה.

## כיצד לבצע

מתן תאריך בעתיד או בעבר בשפת גלים נעשה באמצעות הפונקציה `Date.calculate/3`. ניתן לפרט את התאריך האיתנו, יחידת הזמן (ימים, שבועות, חודשים וכו') והמספר המבוקש. לדוגמה:

```Gleam
let start_date = Date.create(2021, 6, 1)
let future_date = Date.calculate(start_date, WeekUnits, 2)
let past_date = Date.calculate(start_date, MonthUnits, -1)
```

תוצאה:

```Gleam
future_date = {2021, 6, 15}
past_date = {2021, 4, 1}
```

## עוד על חישוב תאריך בעתיד או בעבר

כאשר משתמשים בפונקציה `Date.calculate/3`, חשוב לזכור שיחידת הזמן שנבחרה תשפיע על התוצאה הסופית. כמו כן, יש לקחת בחשבון גם מה יהיו התאריכים האחראיים לכל יחידת זמן כדי לאתר את התאריך הנדרש.

## ראה גם

- התיעוד הרשמי של פונקציית `Date.calculate/3` בשפת גלים: https://gleam.run/docs/standard-library/date.html#calculate
- כיצד להשתמש בפונקציה `Date.create/3` כדי ליצור תאריך: https://gleam.run/docs/standard-library/date.html#create