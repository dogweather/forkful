---
title:    "PHP: הדפסת פלט מנקודת נוף שלא תקינה"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/php/printing-debug-output.md"
---

{{< edit_this_page >}}

"## למה"

תכנות ב-PHP הינו פעילות מאתגרת ומרתקת, אבל לעתים קרובות נתקלים בבעיות ושגיאות שמצריכות חיפוש ותיקון. כאשר מדובגים את הקוד, יש ניסיון נוסף להבין את התהליך ולמצוא את השגיאה. כתיבת פלט דיבאג עם הקוד יכולה לסייע בזה ולהפוך את התהליך לפשוט יותר.

"## איך לעשות זאת"

קוד PHP שמיועד להצפנה (encryption) ופעולות דיבאג ביחד עלול לבלבל משתמשים. למזלנו, ניתן להדפיס פלט דיבאג שינוח עבורנו ויסייע בזיהוי הבעיות. לצורך זה, אנו משתמשים בפונקציות כגון `print_r()` או `var_dump()` שיש להם כמה פרמטרים שבאמצעותם אנו מציגים מידע נוסף על ערך מסוים. לדוגמה:

```PHP
$array = array("apple", "banana", "orange");
print_r($array);
```

הפלט יהיה:

```
Array
(
    [0] => apple
    [1] => banana
    [2] => orange
)
```

"## טיול עמוק"

לעתים קרובות עלינו להתמודד עם באגים קשים שמקורם לא ברור. במקרים כאלה, הדפסת פלט דיבאג עלולה להראות לנו מידע נוסף על המשתנה או הערך שמפיק את הבעיה. ניתן להשתמש בפונקציות דיבאג כמו `var_dump()` על מנת להציג מידע נוסף על הבעיה והקלט שהתקבל. בכך נוכל להבין טוב יותר מה הולך במקום הלא נכון ולתקן את הבאג.

"See Also"
- [Debugging PHP with print_r and var_dump](https://www.w3schools.com/php/php_debugging.asp)
- [The Power of PHP Debugging with var_dump and print_r](https://www.phpjabbers.com/the-power-of-php-debugging-with-var_dump-and-print_r-php80.html)
- [Debugging and Troubleshooting in PHP](https://www.linux.com/tutorials/debugging-and-troubleshooting-in-php/)