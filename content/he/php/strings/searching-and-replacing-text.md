---
title:                "חיפוש והחלפת טקסט"
aliases:
- /he/php/searching-and-replacing-text/
date:                  2024-01-20T17:59:04.341153-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט ב-PHP זה כמו חידוש לקסט שלך - אתה מחפש משהו מסוים ומחליף אותו במשהו אחר. מתכנתים עושים את זה כדי לעדכן נתונים, לתקן שגיאות, או לעבד טקסט באופן יעיל.

## איך לעשות:
```PHP
<?php
// בסיסי: str_replace
$originalString = "שלום עולם! PHP היא כיף.";
$replacedString = str_replace("כיף", "אדירה", $originalString);
echo $replacedString; // יוצא "שלום עולם! PHP היא אדירה."

// רגולרי אקספרשנס: preg_replace
$regexString = "PHP היא [א-ת]+.";
$resultString = preg_replace("/PHP היא [א-ת]+./", "PHP היא פטיש!", $originalString);
echo $resultString; // יוצא "שלום עולם! PHP היא פטיש!"
?>
```

## צלילה לעומק
חפש-והחלף הוא כלי עתיק יומין בתכנות. הפונקציה `str_replace` הגיעה עם הגרסאות הראשונות של PHP - פשוטה ויעילה לשימוש מיידי. כשהדברים מתחילים להיות מסובכים יותר, רגולרי אקספרשנס נכנסת לתמונה עם `preg_replace`, שמאפשר ליצור חיפושים פחות סטטיים ויותר מורכבים. זכרו, עם כוח גדול באה אחריות גדולה; שימוש לא נכון ב-regex עלול להוביל לבאגים מוזרים.

## ראו גם
- [PHP: str_replace - Manual](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace - Manual](https://www.php.net/manual/en/function.preg-replace.php)
- [Regular Expressions Info](https://www.regular-expressions.info/)
- [PHP: The Right Way - Regular Expressions](https://phptherightway.com/#regular_expressions)
