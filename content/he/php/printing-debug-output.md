---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
יציאת דיבוג היא שיטה עליה משתמשים מתכנתים כדי להציג ערכים ממשתנים במהלך ריצת הקוד. זה מסייע לזיהוי תקלות וניתוח התנהגות הקוד.

## איך: 
נגיע לקוד. שימו לב לראות את הפונקציה var_dump, שהיא המנגנון המרכזי שאנחנו משתמשים בפוטציאל:

```PHP
<?php
   $a = array(1, 2, array("a", "b", "c"));
   var_dump($a);
?>
```

כשנריץ את הקוד הזה, התוצאה תהיה:

```
array(3) {
  [0]=> int(1)
  [1]=> int(2)
  [2]=> array(3) {
    [0]=> string(1) "a"
    [1]=> string(1) "b"
    [2]=> string(1) "c"
  }
}   
```

## שימוע מעמיק:
var_dump הוא נכס ישן במערכת PHP, נוסף ב- PHP 4. חלופות מודרניות כוללות print_r ו var_export, שיוצר מחרוזת של פורמט משחרר.

אם אתם מחפשים להדפיס פלט מדיבוג בלי להרביץ את המשתמש הסופי שלכם, אתם יכולים להשתמש בפונקציית error_log, שתכתוב את הפלט לקובץ לוג במקום לשלוח אותו לדפדפן.

## ראו גם: 
אתם בהחלט מוזמנים לשם הבנה עמוקה יותר של שימוש במכניזמים אלה, להתחיל לחפש משאבים נוספים על האינטרנט. מרכזי ההתחלה הטובים ביותר הם:

- [מדריך הפונקציה error_log ב-PHP.net](https://www.php.net/manual/en/function.error-log.php)
- [מדריך הפונקציה var_dump ב-PHP.net](https://www.php.net/manual/en/function.var-dump.php)