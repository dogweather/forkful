---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?

החיפוש וההחלפה של טקסט הם פעולות בסיסיות שמתכנתים בוצעים בטקסט. הם חיוניים כדי לשפר את היכולת לגשת למידע ולשנותו באופן יעיל.

## איך ל:

הינה דוגמה לחיפוש והחלפה של "שפה" ב- "תכנות" באמצעות PHP:

```PHP
<?php
$txt = "אני אוהב לתכנת בשפה העברית";
echo str_replace("שפה", "תכנות", $txt);
?>
```

יוצא:

```
אני אוהב לתכנת בתכנות העברית
```

## צלילה עמוקה:

1. היסטוריה: הפונקציה str_replace נוצרה ב(PHP 4) ונמשיכה ליהות עד היום.
2. חלופות: ב- PHP, גם הפונקציה preg_replace() מתוקננת לביצוע החלפות בדפוסים מורכבים יותר.
3. פרטי הפעלה: str_replace() מחפשת את המחרוזת הכלולה בארגומנט הראשון ומחליפה אותה במחרוזת מהארגומנט השני במחרוזת המקור שמועברת בארגומנט השלישי.


## ראה גם:

- [מדריך PHP.NET str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [מדריך PHP.NET preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)