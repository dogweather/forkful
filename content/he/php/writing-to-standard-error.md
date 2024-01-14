---
title:    "PHP: כתיבה לטעות תקן"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## למה

כתיבה לשגרת השגיאה היא כלי חיוני בתכנות פי.פי. העבודה עם השגרת השגיאה מאפשרת לספק מידע ואזהרות חשובות למפתחי התוכנה וכן למשתמשים.

## כיצד לעשות זאת

בקוד המצורף ניתן לראות דוגמא לשימוש בפונקציית `error_log` כדי להדפיס מידע לשגיאה בצורה דינמית ומותאמת לשפת המשתמש באמצעות הפרמטר `message`:

```PHP
<?php
$error_message = "התרחשה שגיאה בשרת האינטרנט";
error_log($error_message);
?>
```

פלט השגיאה יופיע במיקום המוגדר לשגרת השגיאה בשרת האינטרנט, וכן יתפרסם בכל חלון השגיאות של התוכנית.

## Deep Dive

לשימוש בפונקציה `error_log` ישנם פרמטרים רבים נוספים שניתן להגדיר, כגון רמת השגיאה, תאריך וזמן הגילוי וכו'. כך ניתן לפרט ולהתאים את ההצגה של השגיאה לפי הצורך ולטפל במצבים מיוחדים כמו חריגות ותקלות.

## ראה גם

- [מדריך לפונקציות שליחת הודעות שגיאה בפי.פי](https://www.php.net/manual/en/ref.errorfunc.php)
- [הסבר על שימוש בשגרת השגיאה בפי.פי](https://www.php.net/manual/en/function.error-log.php)
- [דוגמאות נוספות לשימוש בפונקציית `error_log`](https://www.w3schools.com/php/func_error_log.asp)