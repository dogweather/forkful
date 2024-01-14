---
title:    "PHP: מחיקת תווים התואמים לדפוס"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## למה

למה למחוק אותיות התואמות תבנית? יתרון חשוב במחיקת תוויות מסוימים הוא כדי לנקות או לעקוב לאחר אבטחת מאגרי נתונים. 

## איך לעשות זאת

```PHP
$str = "זהו מחרוזת עם תווים שלא נרצה להציג.";
echo preg_replace("/[^a-zא-ת]/i","", $str);
```

הפלט יהיה: זהומחרוזתעםתוויםשלאנרצהלהציג

פונקציית קולב PHP preg_replace() מאפשרת מחיקת תווים התואמים תבנית ספציפית באמצעות שימוש בביטויים רגולריים. זה יכול להיות שימושי במיוחד כאשר מחפשים לנקות מחרוזות שנכתבו עם תווים לא תקינים או להסיר תווים מסוימים.

## היכנס לעומק

פונקציית קולב preg_replace() מקבלת שלושה פרמטרים: תבנית, תלוי, ומחרוזת. תבנית מכילה את התווים הספציפיים שתרצה למחוק, התלוי מכיל את התיווך שיש למחוק במקרה שהתווים אותם אתה מחפש לא נמצאים, והמחרוזת היא המחרוזת לכיוון שתיתן את התחליף. ניתן להשתמש בביטויים רגולריים מתקדמים כדי להעביר את התבנית, תלוי, והמחרוזת לרמת אחרת של מיקוד. 

## ראה גם

- [PHP preg_replace() פונקציה](https://www.php.net/manual/en/function.preg-replace.php)
- [RegExr: אתר לבדיקת ביטויים רגולריים באתר](https://regexr.com/)