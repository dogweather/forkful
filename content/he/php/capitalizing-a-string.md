---
title:                "הגדלת אותיות במחרוזת"
html_title:           "PHP: הגדלת אותיות במחרוזת"
simple_title:         "הגדלת אותיות במחרוזת"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# מה ולמה?
אילוץ אותיות במחרוזת מתייחס לפעולה של שינוי אות הראשונה בכל מילה לאות גדולה. מתכנתים משתמשים בזה למטרות עיצוב וקריאות, תוך שמירה על חוקי התחביר של שפה מסוימת.

# איך לעשות את זה:
עיבוד מחרוזות ב-PHP הוא משימה פשוטה עם הפונקציה ucwords().

```PHP
$text = "מה היום השבוע?";
$capitalizedText = ucwords($text);
echo $capitalizedText;
```

הפלט של קוד זה יהיה: "מה היום השבוע?" - שימו לב שכל אות ראשונה בכל מילה הפכה לאות גדולה.

# צלילה עמוקה:
אילוצי שפות אחרות (למשל, שפות שמשתמשות באותיות לועזיות) מוסיפות אות גדולה ראשונה لלמילה בלבד. אבל, זה לא עובד עם כל השפות. לדוגמה, עברית לא משתמשת בחוקי Case כך ש ucwords() לא ישנה ראשי תיבות. כדי לטפל בראשי תיבות בשפות אחרות, תוכל ליצור פונקציה משלך.
אלטרנטיבית, אם אתה מטפל במחרוזות גדולות, varchar() יהיה יותר מהיר.

# ראה גם:
1. [PHP: ucwords](https://www.php.net/manual/en/function.ucwords.php)
2. [PHP for Beginners by PHP Official Site](https://www.php.net/manual/en/tutorial.php)
3. [PHP String functions by W3Schools](https://www.w3schools.com/php/php_string.asp)