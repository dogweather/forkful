---
title:                "PHP: מחיקת תווים תואמים לתבנית"
simple_title:         "מחיקת תווים תואמים לתבנית"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

חשיבה אחרית:למה מישהו ירצה למחוק תווים התואמים לתבנית:

זאת פעולה נפוצה בתכנות של מערכות ווב או אפליקציות של מסדי נתונים, כאשר יש צורך להסיר יישויות עם מידע מיוחד כגון סיסמאות או כתובות מייל רגישות.

### איך לעשות:

הנה דוגמא לקוד PHP פשוט המסביר איך למחוק תווים התואמים לתבנית באמצעות הפונקציה `preg_replace`:

```PHP
// מחרוזת עם תווים למחיקה
$string = "מחרוזת עם תווים למחיקה_1_2_3";

// הביטוי הרגולרי למחיקת אלמנטים שהם ספרות
$pattern = "/[0-9]+/";

// המחרוזת המעודכנת עם התווים המתאימים
$new_string = preg_replace($pattern, "", $string);

echo $new_string;

// תוצאה: "מחרוזת עם תווים למחיקה_"
```

### מעמקים:

ניתן להשתמש בביטויים רגולריים למחיקה של מספרים, אותיות, תווים מיוחדים ועוד. בנוסף, ניתן להשתמש בפונקציות נוספות כמו `preg_replace_callback` שתאפשר לבצע פעולות מתקדמות יותר כמו גיבוב של מספרים או תאריכים.

### ראו גם:

- [פונקציית preg_replace במדריך רשמי של PHP](https://www.php.net/manual/en/function.preg-replace.php)
- [מדריך מלא על ביטויים רגולריים בPHP](https://www.php.net/manual/en/reference.pcre.pattern.syntax.php)
- [דוגמאות לשימוש בביטויים רגולריים להתאמת תבניות](https://www.r-exercises.com/regex-findall/)