---
date: 2024-01-20 17:33:39.587156-07:00
description: "\u05DC\u05D4\u05E9\u05D5\u05D5\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D1\
  \u05D3\u05D5\u05E7 \u05D0\u05D9\u05D6\u05D4 \u05DE\u05D4\u05DD \u05DE\u05D5\u05E7\
  \u05D3\u05DD \u05D9\u05D5\u05EA\u05E8, \u05D0\u05D5 \u05D0\u05DD \u05D4\u05DD \u05D0\
  \u05D5\u05EA\u05D5 \u05D3\u05D1\u05E8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9\
  \ \u05DC\u05D0\u05E8\u05D2\u05DF \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD, \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD, \u05D0\u05D5 \u05DC\u05D1\u05E6\u05E2 \u05E4\u05E2\
  \u05D5\u05DC\u05D5\u05EA \u05D1\u05D6\u05DE\u05E0\u05D9\u05DD \u05DE\u05E1\u05D5\
  \u05D9\u05DE\u05D9\u05DD."
lastmod: '2024-03-13T22:44:39.501591-06:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D4\u05E9\u05D5\u05D5\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D1\u05D3\
  \u05D5\u05E7 \u05D0\u05D9\u05D6\u05D4 \u05DE\u05D4\u05DD \u05DE\u05D5\u05E7\u05D3\
  \u05DD \u05D9\u05D5\u05EA\u05E8, \u05D0\u05D5 \u05D0\u05DD \u05D4\u05DD \u05D0\u05D5\
  \u05EA\u05D5 \u05D3\u05D1\u05E8."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
weight: 27
---

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
