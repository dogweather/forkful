---
title:                "קריאת פרמטרים משורת הפקודה"
date:                  2024-01-20T17:56:39.042007-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה היא התהליך בו אנו מאפשרים לתוכנה שלנו לקבל קלט מהמשתמש דרך הטרמינל. תכניתים עושים זאת כדי לאפשר אינטרקציה דינמית והתאמה אישית של ההתנהלות של הסקריפט ללא הצורך בשינוי הקוד.

## איך לעשות:
```php
<?php
// שמירת רשימת הארגומנטים במשתנה
$args = $argv;

// הדפסת הארגומנט הראשון (לאחר שם הסקריפט)
echo "הארגומנט הראשון הוא: " . $args[1] . "\n";
?>
```
פלט לדוגמה, כאשר מריצים את הסקריפט עם הארגומנט 'שלום':
```
הארגומנט הראשון הוא: שלום
```

## צלילה עמוקה
ב-PHP, רשימת ארגומנטים מהקומנד ליין נגישה דרך המשתנה המיוחד `$argv`, שזהו מערך עם הנתונים. `$argv[0]` תמיד יהיה שם הסקריפט, אז הארגומנטים שהמשתמש מזין מתחילים מ-$argv[1]. היסטורית, קריאת ארגומנטים הייתה חלק מתכנות ברוב שפות תכנות לפנים, משום שהיא נותנת גמישות רבה לבצע פעולות בהתאם לצורך הרגעי. חלופה לשיטה זו היא קריאת קלט מתוך קובץ קונפיגורציה או ממסד נתונים, אך זה לא תמיד מתאים כשדורשים גמישות מיידית. מבחינת פרטי היישום, תמיד כדאי לבדוק את הארגומנטים לפני שמשתמשים בהם - להגן מפני שגיאות וגם מפני קלט זדוני.

## ראה גם
- התיעוד הרשמי של PHP על ארגומנטים בשורת פקודה: [https://www.php.net/manual/en/reserved.variables.argv.php](https://www.php.net/manual/en/reserved.variables.argv.php)
- הדרכה למתכנתי PHP על קבלת ארגומנטים מקומנד ליין: [https://www.php.net/manual/en/features.commandline.php](https://www.php.net/manual/en/features.commandline.php)