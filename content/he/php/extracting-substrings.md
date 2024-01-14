---
title:                "PHP: חילוץ מחרוזות מאמר על תיכנות מחשבים"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/extracting-substrings.md"
---

{{< edit_this_page >}}

### למה

מילים משמעותיות בעברית

### איך לעשות

```PHP
$string = "שלום עולם";
```

```PHP
echo substr($string, 6); // עולם
echo substr($string, 2, 3); // לום
```

### חפירה עמוקה

"substr" הוא פונקציה שימושית כאשר יש צורך להפעיל על חלק מהמחרוזת בלבד. פונקציה זו מקבלת שני פרמטרים, ראשון הוא המחרוזת והשני הוא המיקום הראשון של מחרוזת התת. אפשר להוסיף פרמטר נוסף למצוא את מירשומי המחרוזת בטווח מסוים.

### ראה גם

- מידע נוסף על "substr" פונקציה: https://www.php.net/manual/en/function.substr.php
- רשימת פונקציות נוספות שיכולות לסייע בעיבוד מחרוזות: https://www.geeksforgeeks.org/php-string-functions/