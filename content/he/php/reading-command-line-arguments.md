---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# מה ולמה?
הקריאה לארגומנטים של שורת פקודות היא אופן בו תוכנית מקבלת נתונים מפקודות המצוים בשורת הפקודה של המערכת. תכניתים מתןשאים בכך כדי לאפשר גמישות בתחום של התכנית ולשנות את ההתנהגות שלה נכון לצורך.

# איך ל־:
קוד דוגמא ופלט בקוד ה-PHP שלכם:

```PHP 
<?php
// $argv is an array that contains the command-line arguments
print_r($argv);
?>
```

אם תריצו את התכנית הבאה עם פקודה שכוללת פרמטרים (`php script.php arg1 arg2`) הפלט יהיה:

```PHP 
Array
(
    [0] => script.php
    [1] => arg1
    [2] => arg2
)
```

# התמיכה המעמיקה:
האינטרפטר של PHP משתמש במשתנה הכבוי $argv כדי לאגור את הארגומנטים שהועברו על ידי שורת הפקודה. זה הוא דרך מסורתית שאותה קיבלו שפות תכנות אחרות מ-C. אם אתם רוצים אופציה שתתמוך בדרישות מורכבות יותר, כמו פרסורי אופציה, ראו את הספריות הנלמדות כמו getopt() או את Symfony Console.

# ראו גם:
• PHP CLI SAPI documentation: https://www.php.net/manual/en/features.commandline.php
• Symfony Console component: https://symfony.com/doc/current/components/console.html
• PHP `getopt()` documentation: https://www.php.net/manual/en/function.getopt.php