---
title:                "PHP: כתיבה לתקליט הסטנדרטי"
simple_title:         "כתיבה לתקליט הסטנדרטי"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

למה לכתוב לתקליטור המיועד לשגיאות?

כתיבה לתקליטור המיועד לשגיאות (stderr) היא כלי עזר חשוב כאשר מתכנתים מעורבים בקוד פיתוח. זה מאפשר להם לזהות במהירות שגיאות ובאופן פשוט להבין כיצד קודם התייחסה למערכת.

## איך לעשות זאת

באמצעות פקודת ה-PHP "error_log". ניתן להשתמש בפקודה זו כדי לכתוב מסרים לתקליטור המיועד לשגיאות ולראות את התוצאה בתוכנת הטקסט המתאימה. לדוגמא:

```PHP
// כתיבת מסר לתקליטור המיועד לשגיאות
$error_message = "אופס! משהו השתבש";
error_log($error_message);
```

תוצאה:

```
[תאריך ושעה] [error]  אופס! משהו השתבש
```

## מעמד עמוק

פקודת ה-PHP "error_log" מוגדרת על ידי המשתמש ומאפשרת לו להגדיר איזה מסרים יופיעו בתקליטור המיועד לשגיאות. ניתן להשתמש בפקודה זו גם כדי לשגר מסרים לאתר האינטרנט האישי של המשתמש. כדי להגביר את היעילות, ניתן להגדיר את הפקודה בקוד הראשי של האתר כדי להבטיח שהמסרים יתווספו לתקליטור בכל זמן מעבר בתוך האתר.

## ראו גם

- [מסמכי PHP רשמיים](https://www.php.net/manual/he/index.php)
- [מדריך לפקודת PHP "error_log"](https://www.php.net/manual/he/function.error-log.php)
- [כיצד לנהל תקליטורים ושגיאות באתרי PHP](https://www.phpgang.com/how-to-manage-logs-amp-errors-in-php-sites_94.html)