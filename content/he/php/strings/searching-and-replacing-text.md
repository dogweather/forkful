---
date: 2024-01-20 17:59:04.341153-07:00
description: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D1-PHP \u05D6\u05D4 \u05DB\u05DE\u05D5 \u05D7\u05D9\
  \u05D3\u05D5\u05E9 \u05DC\u05E7\u05E1\u05D8 \u05E9\u05DC\u05DA - \u05D0\u05EA\u05D4\
  \ \u05DE\u05D7\u05E4\u05E9 \u05DE\u05E9\u05D4\u05D5 \u05DE\u05E1\u05D5\u05D9\u05DD\
  \ \u05D5\u05DE\u05D7\u05DC\u05D9\u05E3 \u05D0\u05D5\u05EA\u05D5 \u05D1\u05DE\u05E9\
  \u05D4\u05D5 \u05D0\u05D7\u05E8. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\
  \u05D3\u05DB\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05EA\u05E7\u05DF\
  \ \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA, \u05D0\u05D5 \u05DC\u05E2\u05D1\u05D3 \u05D8\
  \u05E7\u05E1\u05D8\u2026"
lastmod: '2024-02-25T18:49:37.697221-07:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8 \u05D1-PHP \u05D6\u05D4 \u05DB\u05DE\u05D5 \u05D7\u05D9\u05D3\
  \u05D5\u05E9 \u05DC\u05E7\u05E1\u05D8 \u05E9\u05DC\u05DA - \u05D0\u05EA\u05D4 \u05DE\
  \u05D7\u05E4\u05E9 \u05DE\u05E9\u05D4\u05D5 \u05DE\u05E1\u05D5\u05D9\u05DD \u05D5\
  \u05DE\u05D7\u05DC\u05D9\u05E3 \u05D0\u05D5\u05EA\u05D5 \u05D1\u05DE\u05E9\u05D4\
  \u05D5 \u05D0\u05D7\u05E8. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D3\
  \u05DB\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05EA\u05E7\u05DF \u05E9\
  \u05D2\u05D9\u05D0\u05D5\u05EA, \u05D0\u05D5 \u05DC\u05E2\u05D1\u05D3 \u05D8\u05E7\
  \u05E1\u05D8\u2026"
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
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
