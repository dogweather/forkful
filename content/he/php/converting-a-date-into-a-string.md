---
title:                "המרת תאריך למחרוזת"
date:                  2024-01-20T17:37:31.465547-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת תאריך למחרוזת"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת ב-PHP זו פעולה שבה אנחנו לוקחים ערך תאריך (אובייקט DateTime, טיימסטמפ וכו') והופכים אותו לטקסט. זה מועיל כשרוצים להציג או להתאים תאריכים לפורמט ספציפי, לתעדוף או לשליחת נתונים.

## איך עושים את זה:
כדי להמיר תאריך למחרוזת, אפשר להשתמש בפונקציית `date` או במתודת `format` של אובייקט DateTime. קבלו דוגמה:

```php
<?php
// שימוש בפונקציית date
echo date('Y-m-d'); // יוצא תאריך נוכחי בפורמט YYYY-MM-DD

// שימוש באובייקט DateTime והמתודה format
$date = new DateTime('now');
echo $date->format('Y-m-d H:i:s'); // יהפוך את התאריך למחרוזת עם זמן
?>
```

תוצאה:
```
2023-03-15
2023-03-15 14:35:52
```

## עומק של ידע
ב-PHP, המרת תאריך למחרוזת החלה עם הפונקציה `strftime`, שהפכה לא פעילה ב-PHP 8.0. כיום, אנחנו עובדים עם הפונקציה `date` ואובייקט DateTime. אם יש צורך להמיר תאריכים מפורמט אחד לפורמט אחר, אפשר לעשות זאת בחופשיות עם מתודת `format`. כמו כן, ישנה המחלקה `DateInterval` לפעולות מתקדמות יותר עם תאריכים.

אלטרנטיבות כוללות ספריות חיצוניות כמו Carbon ב-PHP, שמספקת עוד שלל פונקציות לניהול תאריכים. לעומק, כל פורמט שאנחנו מציינים בפונקציות אלו מבוסס על תקני תאריכים של PHP, שניתן למצוא בתיעוד הרשמי.

## ראה גם:
- [תיעוד PHP לפונקציית date](https://www.php.net/manual/en/function.date.php)
- [תיעוד PHP למחלקת DateTime](https://www.php.net/manual/en/class.datetime.php)
- [תקני פורמט של תאריכים מרשמי תיעוד PHP](https://www.php.net/manual/en/datetime.format.php)
- [אתר Carbon](https://carbon.nesbot.com/docs/)
