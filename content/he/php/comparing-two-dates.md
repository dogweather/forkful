---
title:    "PHP: השוואת שתי תאריכים."
keywords: ["PHP"]
---

{{< edit_this_page >}}

## למה
במרבית האתרים והיישומים שמשתמשים בתאריכים, יש צורך להשוות בין שני תאריכים, כדי לבדוק את הסדר שלהם או לבדוק אם נמצאים באותו יום. תהליכים אלו בילוי לרוב מאוד עם `strtotime()` ויש לו את חוקיות של ניקוד ותיוג בהידורי תאריכים בפורמטים כמו אלו המצויים ברפואה או שבו עבדת עם מושגים כגון עבוד נטינית או זמן מאוחר יותר בתרגומים בין-לאומיים.

## כיצד לעשות זאת
דוגמאות לקוד ולפלט המדגימות את כיצד ניתן להשוות בין שני תאריכים באמצעות `strtotime()`:

```PHP
// יצירת תאריכים
$date1 = strtotime("2020-01-01");
$date2 = strtotime("2020-02-01");

// השוואה בין התאריכים
if ($date1 < $date2) {
    echo "התאריך הראשון מוקדם יותר מהשני";
} elseif ($date1 > $date2) {
    echo "התאריך השני מוקדם יותר מהראשון";
} else {
    echo "התאריכים זהים";
}
```

פלט:

```
התאריך הראשון מוקדם יותר מהשני
```

## Deep Dive
כדי להשוות בין שני תאריכים באמצעות `strtotime()`, יש להתחבר לספריית Standard PHP Library (SPL) כדי לקרוא למרחב המילונים כך שניתן ומותג את תיבת העצמים אליה כאשר נעבור על התיבות. זה נקרא מכפילת הרשימות כאשר תיבות נחיקים מתוך מילונים אלה באמצעות רץ באמת כשדווה ספרייר המילונים מרחבים בשימוש במכין. Advanced Calendar, lichman.net, timelib and cron.write, all of which use display dates between two formats and when one might happen before the other depending on the selected location and popularity of the used dates.

## הצג גם
- [PHP.net: מסמכים נוספים על `strtotime()`](https://www.php.net/manual/en/function.strtotime.php)
- [מדריך רשמי המבוא לPHP על תאריכים](https://www.php.net/manual/en