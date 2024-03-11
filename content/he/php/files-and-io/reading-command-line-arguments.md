---
date: 2024-01-20 17:56:39.042007-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D4\u05D9\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05D1\u05D5 \u05D0\u05E0\
  \u05D5 \u05DE\u05D0\u05E4\u05E9\u05E8\u05D9\u05DD \u05DC\u05EA\u05D5\u05DB\u05E0\
  \u05D4 \u05E9\u05DC\u05E0\u05D5 \u05DC\u05E7\u05D1\u05DC \u05E7\u05DC\u05D8 \u05DE\
  \u05D4\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D3\u05E8\u05DA \u05D4\u05D8\u05E8\u05DE\
  \u05D9\u05E0\u05DC. \u05EA\u05DB\u05E0\u05D9\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E4\u05E9\u05E8\
  \ \u05D0\u05D9\u05E0\u05D8\u05E8\u05E7\u05E6\u05D9\u05D4 \u05D3\u05D9\u05E0\u05DE\
  \u05D9\u05EA \u05D5\u05D4\u05EA\u05D0\u05DE\u05D4 \u05D0\u05D9\u05E9\u05D9\u05EA\
  \u2026"
lastmod: '2024-03-11T00:14:12.979078-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D4\u05D9\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05D1\u05D5 \u05D0\u05E0\
  \u05D5 \u05DE\u05D0\u05E4\u05E9\u05E8\u05D9\u05DD \u05DC\u05EA\u05D5\u05DB\u05E0\
  \u05D4 \u05E9\u05DC\u05E0\u05D5 \u05DC\u05E7\u05D1\u05DC \u05E7\u05DC\u05D8 \u05DE\
  \u05D4\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D3\u05E8\u05DA \u05D4\u05D8\u05E8\u05DE\
  \u05D9\u05E0\u05DC. \u05EA\u05DB\u05E0\u05D9\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E4\u05E9\u05E8\
  \ \u05D0\u05D9\u05E0\u05D8\u05E8\u05E7\u05E6\u05D9\u05D4 \u05D3\u05D9\u05E0\u05DE\
  \u05D9\u05EA \u05D5\u05D4\u05EA\u05D0\u05DE\u05D4 \u05D0\u05D9\u05E9\u05D9\u05EA\
  \u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
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
