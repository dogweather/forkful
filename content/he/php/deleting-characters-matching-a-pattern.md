---
title:                "PHP: מחיקת תווים התואמים לתבנית"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

למה: כדי למחוק תווים המתאימים לתבנית מסוימת. זוהי פעולה נפוצה בפיתוח ווב, והיא יכולה לסייע לך לארגן ולנקות נתונים.

איך לעשות זאת: נוכל להשתמש בפעולה המובנית "preg_replace" בשפת פי.אצ'פי כדי למחוק תווים המתאימים לתבנית מסוימת. לדוגמה, ניתן להשתמש בפקודה הבאה כדי למחוק את כל התווים הייתכן ומכילים מספר:

```PHP
$pattern = '/[0-9]+/';
$str = 'Hello123 World456';
$result = preg_replace($pattern, '', $str);
echo $result; // Output: Hello World 
```

עמוק יותר: כדי להבין את השימוש בפונקציה המובנית "preg_replace", חשוב להבין את הכלים המתחברים אליה. תבנית המתאימה משמשת כפרמטר ראשון, והיא יכולה להיות בכל תבנית תווים שברצונך למחוק. המחרוזת המיועדת למחיקה היא הפרמטר השני. בנוסף, באפשרותך להוסיף פרמטר נוסף שהוא המחרוזת החדשה שתחליף את התווים הנמחקים, אם מטרתך היא גם להחליף אותם.

ראה גם: 
- דוגמת קוד לשימוש בפונקציה preg_replace עם תבנית שמזהה טלפון מסוג "ארה"ץ: https://www.php.net/manual/en/function.preg-replace.php 
- הסבר נוסף על פונקציות המחרוזת המובנות בשפת פי.אצ'פי: https://www.php.net/manual/en/ref.strings.php