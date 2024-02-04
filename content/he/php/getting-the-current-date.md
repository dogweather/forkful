---
title:                "קבלת התאריך הנוכחי"
date:                  2024-02-03T19:10:52.727381-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי ב-PHP היא משימה יסודית המאפשרת לך לאחזר ולתמרן את תאריך ושעת המערכת. זה קריטי לפונקציות כמו לוגינג, חותמת זמן לפוסטים, תזמון אירועים, או ביצוע פעולות תלויות זמן באפליקציות שלך.

## איך לעשות:
### PHP טבעי
הפונקציה המובנית `date()` של PHP היא הדרך הישירה ביותר לקבל את התאריך הנוכחי. ניתן לעצב את התאריך בדרכים שונות על ידי הגדרת הפרמטר format.

```php
echo date("Y-m-d"); // מוציא: 2023-04-01 (לדוגמה)
echo date("l, F j, Y"); // מוציא: שבת, אפריל 1, 2023
```

לקבלת התאריך והשעה עם תמיכה באזור זמן, ניתן להשתמש בקלאס `DateTime` יחד עם `DateTimeZone`.

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // מוציא: 2023-04-01 12:00:00 (לדוגמה)
```

### שימוש ב-Carbon (ספרייה צד שלישי פופולרית)
[Carbon](https://carbon.nesbot.com/) היא הרחבת API פשוטה ל-`DateTime` המציעה דרך נקייה וזורמת יותר לעבוד עם תאריכים ושעות.

ראשית, ודא ש-Carbon מותקן דרך Composer:
```bash
composer require nesbot/carbon
```

לאחר מכן, ניתן להשתמש בו לקבלת התאריך הנוכחי:

```php
use Carbon\Carbon;

echo Carbon::now(); // מוציא: 2023-04-01 12:00:00 (לדוגמה, בפורמט ברירת המחדל)
echo Carbon::now()->toDateString(); // מוציא: 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // מוציא: שבת, אפריל 1, 2023
```

Carbon מעשיר את טיפול התאריך-שעה ב-PHP על ידי הוספת קריאות והמון פונקציונליות להשוואה, תמרון ועיצוב של זמנים.
