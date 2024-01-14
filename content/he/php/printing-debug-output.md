---
title:                "PHP: הדפסת פלטי דיבאג"
simple_title:         "הדפסת פלטי דיבאג"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/printing-debug-output.md"
---

{{< edit_this_page >}}

# למה

למה שותפים נוטים להדפיס פלט תיקון במחלקת התכנות? למפעילים יש קוד תיקון בודק כיול וקשוב לדפיס בצורה צפויה תיקון זה? מבחן זה כולל יותר מאחד בטיפוסי נתונים.

# איך

```PHP
echo "סוג Debug נתונים"; // פלט "תשובה:";
var_dump($data_type); // type הוא משתנה זה
var_dump($assoc_array); // array: פלט "תשובה";
```

# פלט
```PHP
סוג Debug נתונים
string(4) "type"
array(3) {
  ["className"] => string(12) "סוג Debugגלעד 2
  ["formatted"] => string(1) "1"
  ["associative array"] => string(3) "עזרו 1"
}
```

# קפיצה עמוקה

פלט זה מורכב מחלוקה רבה של בקרת פלט לדפס מידע נוסף עם מידע מפורט עבודתה של למשתמשים נוספים בפלט זה.

# ראה גם

- המאמר המקיף על פלט ניתוח תגי HTML אתה יכול למצוא כאן: [PHP: תגי HTML Debug](https://www.php.net/debug)
- יותר עזרה עם פלט תיקון נישמת במאמר מעמיק זה: [PHP תכנות הדפסת תכניתת ומשאבי תיקון](https://www.php.net/debug)
- פתרונות נוספים לבעיות הדפסת תיקון נמעפים למפרט זה: [PHP טכנית הדפסת נתננת תיקון](https://www.php.net/debug)