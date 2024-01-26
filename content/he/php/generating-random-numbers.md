---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:49:58.979429-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? / מה ולמה?
יצירת מספרים אקראיים ב-PHP היא דרך לקבל ערכים שלא ניתן לחזות אותם מראש. מתכנתים משתמשים בזה לטסטים, אבטחה, משחקים, ובחירה רנדומלית.

## How to: / איך לעשות:
בואו ניצור מספר אקראי בין 1 ל-100:

```PHP
<?php
$randomNumber = rand(1, 100);
echo $randomNumber;
?>
```

לדוגמא פלט:

```
42
```

ואם אנחנו רוצים משהו יותר חזק מבחינת אבטחה:

```PHP
<?php
$secureRandomNumber = random_int(1, 100);
echo $secureRandomNumber;
?>
```

דוגמא פלט:

```
57
```

## Deep Dive / צלילה עמוקה:
פונקציית rand() היא הדרך הישנה יותר להפיק מספרים אקראיים ב-PHP, אבל אל תסמכו עליה לצרכי אבטחה - היא לא כל כך חזקה. random_int(), לעומת זאת, משתמשת במערכת ההפעלה לייצר ערכים אקראיים שהם גם בטוחים לשימוש בצורות כמו אימות משתמשים. היא משתמשת במקורות אנטרופיה במערכת ההפעלה, כך שהתוצאה היא קשה יותר לניחוש.

## See Also / ראו גם:
- דוקומנטציה רשמית של PHP על [rand()](https://www.php.net/manual/en/function.rand.php)
- דוקומנטציה רשמית של PHP על [random_int()](https://www.php.net/manual/en/function.random-int.php)
- [OpenSSL](https://www.openssl.org/) לייצור מספרים אקראיים ברמה גבוהה יותר של אבטחה.
