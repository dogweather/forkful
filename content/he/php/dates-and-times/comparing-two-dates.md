---
title:                "השוואת שתי תאריכים"
aliases: - /he/php/comparing-two-dates.md
date:                  2024-01-20T17:33:39.587156-07:00
model:                 gpt-4-1106-preview
simple_title:         "השוואת שתי תאריכים"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
להשוות שתי תאריכים זה פשוט לבדוק איזה מהם מוקדם יותר, או אם הם אותו דבר. תכניתנים עושים את זה כדי לארגן אירועים, נתונים, או לבצע פעולות בזמנים מסוימים.

## איך לעשות:
כדי להשוות תאריכים ב-PHP, תשתמש ב-DateTime או בפונקציות של strtotime. ראה דוגמאות.

```PHP
<?php
$date1 = new DateTime("2023-03-15");
$date2 = new DateTime("2023-03-20");

if ($date1 < $date2) {
    echo "התאריך הראשון מוקדם יותר.";
} else if ($date1 > $date2) {
    echo "התאריך השני מוקדם יותר.";
} else {
    echo "התאריכים זהים.";
}
?>
```

פלט:
```
התאריך הראשון מוקדם יותר.
```

באמצעות strtotime:
```PHP
<?php
$date1 = strtotime("2023-03-15");
$date2 = strtotime("2023-03-20");

if ($date1 < $date2) {
    echo "התאריך הראשון מוקדם יותר.";
} else if ($date1 > $date2) {
    echo "התאריך השני מוקדם יותר.";
} else {
    echo "התאריכים זהים.";
}
?>
```

פלט:
```
התאריך הראשון מוקדם יותר.
```

## טבילה עמוקה
ב-PHP, הטיפול בתאריכים התרחב והשתפר עם השנים. בעבר שימשו פונקציות כמו mktime ו-date, אבל עכשיו מומלץ להשתמש במחלקת DateTime מכיוון שהיא עוזרת להתמודד עם אזורי זמן ופורמטים באופן יותר קל וחזק. שימוש ב-timestamps (כמו שמעביר strtotime) יכול להיות בעייתי בתאריכים מאוד רחוקים בגלל עניין של 32/64 ביטים, אולם DateTime פותרת את בעיות התאימות האלו.

## ראה גם
- [PHP: DateTime - Manual](https://www.php.net/manual/en/class.datetime.php)
- [PHP: strtotime - Manual](https://www.php.net/manual/en/function.strtotime.php)
- [PHP: Date/Time Functions - Manual](https://www.php.net/manual/en/book.datetime.php)
