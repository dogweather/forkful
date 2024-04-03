---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:31.419781-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05DB\u05D9\
  \u05EA\u05D4 \u05D4\u05DE\u05D5\u05D1\u05E0\u05D9\u05EA `DateTime` \u05E9\u05DC\
  \ PHP \u05DE\u05E1\u05E4\u05E7\u05EA \u05E1\u05D8 \u05D7\u05D6\u05E7 \u05E9\u05DC\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DC\u05E2\u05D9\u05D1\u05D5\
  \u05D3 \u05D5\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05EA\u05D0\u05E8\u05D9\
  \u05DB\u05D9\u05DD. \u05E0\u05D9\u05EA\u05DF \u05DC\u05D9\u05E6\u05D5\u05E8 \u05DE\
  \u05D5\u05E4\u05E2 \u05E9\u05DC `DateTime` \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\
  \u05D1\u05E0\u05D0\u05D9,\u2026"
lastmod: '2024-03-13T22:44:39.496422-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05DB\u05D9\u05EA\u05D4 \u05D4\u05DE\u05D5\u05D1\u05E0\u05D9\u05EA\
  \ `DateTime` \u05E9\u05DC PHP \u05DE\u05E1\u05E4\u05E7\u05EA \u05E1\u05D8 \u05D7\
  \u05D6\u05E7 \u05E9\u05DC \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DC\
  \u05E2\u05D9\u05D1\u05D5\u05D3 \u05D5\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD\
  \ \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD."
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

## איך לעשות:
הכיתה המובנית `DateTime` של PHP מספקת סט חזק של פונקציות לעיבוד ועבודה עם תאריכים. ניתן ליצור מופע של `DateTime` ממחרוזת תאריך באמצעות הבנאי, ואז לעצב אותו כפי שנדרש. כך זה נעשה:

```php
$dateString = "2023-04-25 15:30:00";
$dateObject = new DateTime($dateString);

echo $dateObject->format('Y-m-d H:i:s');
// פלט: 2023-04-25 15:30:00
```

כדי לטפל במחרוזות שמתארות פורמטים לא סטנדרטיים, ניתן להשתמש בשיטה `createFromFormat`, המאפשרת לך לציין את הפורמט המדויק של תאריך הקלט:

```php
$dateString = "25-04-2023 3:30 PM";
$dateObject = DateTime::createFromFormat('d-m-Y g:i A', $dateString);

echo $dateObject->format('Y-m-d H:i:s');
// פלט: 2023-04-25 15:30:00
```

לעיבוד מורכב יותר שאולי לא נתמך ישירות על ידי `DateTime`, PHP מציעה את הפונקציה `strtotime`, המנסה לעבד כל תיאור טקסטואלי זמן באנגלית לחותם זמן אוניקס:

```php
$timestamp = strtotime("next Thursday");
echo date('Y-m-d', $timestamp);
// הפלט ישתנה בהתאם לתאריך הנוכחי, למשל, "2023-05-04"
```

**שימוש בספריות צד שלישי:**

למרות שפונקציות המובנות של PHP מכסות מגוון רחב של תרחישי שימוש, לפעמים ייתכן שתזדקקו ליכולות עיבוד מתוחכמות יותר. הספרייה Carbon, המרחיבה את כיתת ה-DateTime של PHP, מספקת סט עשיר של תכונות למניפולציה של תאריך/זמן:

```php
require 'vendor/autoload.php';

use Carbon\Carbon;

$dateString = "Tomorrow";
$date = Carbon::parse($dateString);

echo $date->toDateTimeString();
// הפלט ישתנה, למשל, "2023-04-26 00:00:00"
```

השיטה `parse` של Carbon יכולה לטפל בחכמה במגוון רחב של פורמטים של תאריכים וזמנים, מה שהופך אותה לכלי חשוב עבור אפליקציות הדורשות פונקציונליות גמישה של עיבוד תאריכים.
