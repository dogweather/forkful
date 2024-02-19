---
aliases:
- /he/php/getting-the-current-date/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:52.727381-07:00
description: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\
  \u05E0\u05D5\u05DB\u05D7\u05D9 \u05D1-PHP \u05D4\u05D9\u05D0 \u05DE\u05E9\u05D9\u05DE\
  \u05D4 \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05D4\u05DE\u05D0\u05E4\u05E9\u05E8\
  \u05EA \u05DC\u05DA \u05DC\u05D0\u05D7\u05D6\u05E8 \u05D5\u05DC\u05EA\u05DE\u05E8\
  \u05DF \u05D0\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\u05E2\u05EA \u05D4\
  \u05DE\u05E2\u05E8\u05DB\u05EA. \u05D6\u05D4 \u05E7\u05E8\u05D9\u05D8\u05D9 \u05DC\
  \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DB\u05DE\u05D5 \u05DC\u05D5\
  \u05D2\u05D9\u05E0\u05D2, \u05D7\u05D5\u05EA\u05DE\u05EA \u05D6\u05DE\u05DF \u05DC\
  \u05E4\u05D5\u05E1\u05D8\u05D9\u05DD, \u05EA\u05D6\u05DE\u05D5\u05DF \u05D0\u05D9\
  \u05E8\u05D5\u05E2\u05D9\u05DD,\u2026"
lastmod: 2024-02-18 23:08:52.942869
model: gpt-4-0125-preview
summary: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9 \u05D1-PHP \u05D4\u05D9\u05D0 \u05DE\u05E9\u05D9\u05DE\u05D4\
  \ \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05D4\u05DE\u05D0\u05E4\u05E9\u05E8\u05EA\
  \ \u05DC\u05DA \u05DC\u05D0\u05D7\u05D6\u05E8 \u05D5\u05DC\u05EA\u05DE\u05E8\u05DF\
  \ \u05D0\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\u05E2\u05EA \u05D4\u05DE\
  \u05E2\u05E8\u05DB\u05EA. \u05D6\u05D4 \u05E7\u05E8\u05D9\u05D8\u05D9 \u05DC\u05E4\
  \u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DB\u05DE\u05D5 \u05DC\u05D5\u05D2\
  \u05D9\u05E0\u05D2, \u05D7\u05D5\u05EA\u05DE\u05EA \u05D6\u05DE\u05DF \u05DC\u05E4\
  \u05D5\u05E1\u05D8\u05D9\u05DD, \u05EA\u05D6\u05DE\u05D5\u05DF \u05D0\u05D9\u05E8\
  \u05D5\u05E2\u05D9\u05DD,\u2026"
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
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
