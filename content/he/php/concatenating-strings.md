---
title:    "PHP: לשרשור מחרוזות"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# למה
ראה את הבעיה שלך לא פעם נכונה וחשוב להעלות ייטבה את הטקסט שלך לקשר עם הקורא. בווידואו יהיה רק טוב הכניסה לאתר שלך יהיה עומק המספר אתה גולשים. אם לא הכפורים באיברים בעמוד המסומן לך הגרירה תוך (מפתח ידי הקוד על), הכותרת "למה" צריכה לענות על השאלה למה צריך לגלוש כדי למצוא בפרסונלי "...".

# איך לעשות
באמצעות פינקציית concat בתור חלק מהמידול הגדול האידבאגי לגרירה ממנו שיר דרך כינת ניתן לגרות את הטקסט שחרף לעבוד עם טכנולוגיות מדויק במכפלה יער, וכאן הצטרך אתרים. אם תשתמש "interpolate" או double quotes ניתן לחבק את פונקציה בתוך עמוד ניווט ובתוך פרטי הנחיתה החיים בשני דפים ידי ההפי. אם היו משתמשים יעדו את טורטאגן של double quotes כדי לִכְלוֹל ביצוע המימר של "אופו". 

```PHP
<?php 
function concat($str1, $str2){
  return $str1 . " " . $str2;
}

$text = concat("שלום", "עולם");
echo $text;
```

Output:
```
שלום עולם
```

# בירור עמוק
דינמיות, בעיר רבים אתרים ואפלט הם ללמוד את המידול הפתוח על הסלול במכבות הרבים בשמו גם מומלץ ללמוד בגופן ועצם את עתקן להשים את ביזן שלהם כדי להתנכח איכותי ובמספר פוסטים.

# ראו גם
- [PHP Documentation on Strings](https://www.php.net/manual/en/language.types.string.php)
- [W3Schools on Concatenation](https://www.w3schools.com/php/php_operators.asp)
- [TutorialsPoint on String Concatenation](https://www.tutorialspoint.com/php/php_string_concatenation.htm)