---
title:                "קריאת קובץ טקסט."
html_title:           "PHP: קריאת קובץ טקסט."
simple_title:         "קריאת קובץ טקסט."
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

קובץ טקסט הוא כלי חשוב בתכנות ב-PHP. קריאת קבצים יכולה לסייע בקריאה והצגת מידע מאורגן, כמו טקסט, תמונות או נתונים מבסיס נתונים.

## איך לעשות זאת

להלן כמה דוגמאות קוד ופלטים של קריאת קבצי טקסט ב-PHP:

```
<?php
// פתיחת קובץ טקסט
$file = fopen("example.txt", "r");

// קריאת תוכן הקובץ והדפסתו למסך
echo fgets($file);

// סגירת הקובץ
fclose($file);
?>

פלט:
This is an example of a text file.
```

```
<?php
// קריאת קובץ טקסט והוספת כל שורה למערך
$file = file("example.txt");

// הדפסת כל שורה בתוך המערך
foreach($file as $line) {
  echo $line;
}
?>

פלט:
This is an example of a text file.
```

```
<?php
// קריאת קובץ טקסט והדפסת כל התוכן
echo file_get_contents("example.txt");
?>

פלט:
This is an example of a text file.
```

כדי לקרוא קובץ טקסט, יש להשתמש בפונקציות כמו fopen (), fgets (), file () או file_get_contents (). הפונקציות הללו מאפשרות לקרוא קובץ טקסט ולהתאים אותו לצורכי התוכנה שלך.

## צעדים נוספים

כעת שאתה מכיר את הבסיסיים של קריאת קבצי טקסט ב-PHP, ניתן לעמוד על יסודות אלה וללמוד עוד על פונקציות נוספות כמו fwrite (), fseek () ודומיהן. ישנן גם תיעודים מפורטים ומדריכים זמינים ברשת המסבירים עוד על נושא זה.

## לחקור עוד

למידע נוסף על קריאת קבצי טקסט ב-PHP, בדוק את המדריכים הבאים:

- [מדריך על fopen () ופונקציות נלוות](https://www.w3schools.com/php/func_filesystem_fopen.asp)
- [סיור ב-PHP עם דוגמא