---
title:                "מחיקת תווים התואמים לתבנית"
aliases:
- /he/php/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:54.760048-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
עם פונקציות כמו `preg_replace`, אנחנו מוחקים תווים שמתאימים לתבנית מסוימת. זה שימושי לניקוי קלטים, אימות נתונים ועיבוד טקסט.

## איך לעשות:
```PHP
$text = "שלום! האם למחוק את הסימנים המיוחדים #@$?";
$pattern = '/[#!@$%&*()]/'; // דפוס למחיקה
$clean_text = preg_replace($pattern, '', $text);
echo $clean_text; // תוצאה: "שלום האם למחוק את הסימנים המיוחדים "
```
אפשר גם למחוק תווים בתחומים של ASCII:
```PHP
$text = "Numbers 123, symbols @#₪!, and English abCD";
$pattern = '/[^א-ת ]/'; // [^א-ת ] מתאים לכל דבר שלא תווים בעברית ורווחים
$hebrew_text = preg_replace($pattern, '', $text);
echo $hebrew_text; // תוצאה: " ו"
```

## עיון מעמיק
`preg_replace` מבוססת על ביטויים רגולריים, טכנולוגיה שחלק מהיסודות שלה החלו בשנות ה-50. אלטרנטיבות פשוטות יותר כמו `str_replace` מתאימות למחיקת תווים ספציפיים, אבל לא לתבניות. יש להתייחס לביטויים רגולריים בזהירות כי שגיאה קטנה יכולה לגרום לתוצאות בלתי צפויות או אפילו לתקלות ביצוע.

## ראה גם
- [PHP Manual on preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [Regular Expressions Intro](https://www.regular-expressions.info/)
- [PHP Manual on str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP The Right Way - Regular Expressions](https://phptherightway.com/#regular_expressions)
