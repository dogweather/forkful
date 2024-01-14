---
title:    "PHP: מאחד מחרוזות"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

זה חשוב לדעת איך לשרשר מחרוזות בפHP כי זהו כלי חשוב באתרי אינטרנט דינמיים כדי ליצור תוכן משתנה ודינמי.

## איך לעשות זאת

בשימוש בפקודת `.` בפHP ניתן לשרשר מחרוזות יחד כדי ליצור מחרוזת חדשה. לדוגמה:

```PHP
$name = "שלום";
$greeting = "היי, ";
$new_greeting = $greeting . $name;
echo $new_greeting;
```

פלט: היי, שלום

## לצפייה עמוקה

כדי להבין טוב יותר את שרשור המחרוזות בפHP, חשוב להבין שהפקודה `.` יכולה להיות משתנה ולא רק מחרוזת. זה אומר שניתן לשרשר גם משתנים אחרים, כמו מספרים או מערכים, כדי ליצור מחרוזות מורכבות יותר.

בנוסף, יש לציין שניתן לשרשר כמה מחרוזות בו זמנית, על ידי כתיבת הפקודה `.` רבים כדי לחבר את כל המחרוזות ביחד. לדוגמה:

```PHP
$text1 = "שלום,";
$text2 = "היי,";
$text3 = "איך";
$text4 = "לעבוד";
$full_text = $text1 . $text2 . $text3 . $text4;
echo $full_text;
```

פלט: שלום, היי, איך לעבוד

## ראה גם

למידע נוסף על שרשור מחרוזות בפHP, הנה כמה קישורים שיענו על שאלות אחרות שעשויות לעלות בנוגע לנושא זה:

- [מדריך רשמי של PHP על שרשור מחרוזות](https://www.php.net/manual/en/language.operators.string.php)
- [מאמר מאת W3Schools על שרשור מחרוזות בפHP](https://www.w3schools.com/php/php_operators.asp)
- [וידאו מתארח של ערוץ PHP על כיצד לשרשר מחרוזות בפHP](https://www.youtube.com/watch?v=ELhKIyZa-qk)