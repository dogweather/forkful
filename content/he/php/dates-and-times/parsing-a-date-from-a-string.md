---
title:                "פרסום תאריך ממחרוזת"
aliases:
- /he/php/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:31.419781-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

לעבד תאריך ממחרוזת ב-PHP משמעו להמיר טקסט שמייצג תאריך ו/או שעה לאובייקט `DateTime` של PHP או פורמטים אחרים של תאריך/זמן. זה קריטי לצורך אימות נתונים, מניפולציה, אחסון והצגה, במיוחד כאשר עובדים עם קלט מהמשתמש או נתונים ממקורות חיצוניים.

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
