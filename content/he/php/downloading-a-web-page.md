---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# הורדת דף אינטרנט באמצעות PHP
מאת: עורך לא מכורעני

## מה זה ולמה?
הורדת דף אינטרנט היא הפעולה של שליחה וקבלה של מידע מדי פעם שאתה מבקר באתר. המתכנתים מנצלים את התהליך לצורך **גיבור** או **עדכון מידע** באופן דינאמי.

## איך לעשות: 
הנה קוד בסיסי שממחיש את הרעיון:

```PHP
<?php
$file = file_get_contents('http://website.com');
echo $file;
?>
```

במקרה שלנו, הפונקציה `file_get_contents` מורידה את כל המידע מהדף הנתון. הפלט של הקוד הוא תוכן הדף שאנחנו מורידים.

## בסקירה מעמיקה
PHP שימשה גישה נוחה לגיבוי וגרשת מידע מאז התחלתה ב-1995. אף שיש דרכים חלופיות להשיג את אותו האובייקט (כמו Python או Node.js), PHP ממשיך להיות כלי פופולרי במיוחד עבור אתרי WordPress.

חשוב לציין שהפונקציה `file_get_contents` מתחילה את ההורדה מיד, ולא ניתן לה בטל את פעולת ההורדה או להשפיע עליה בדרכים נוספות כאשר היא "באוויר". אם יש לך ביצועים או טיפול בחריגות מיוחדים, מומלץ לבחור בספריות כמו cURL או Guzzle.

## ראה גם
- [דוקומנטציה רשמית של PHP](https://www.php.net/manual/en/function.file-get-contents.php)
- [אתר המסייע למעבר מ- `file_get_contents` ל-cURL](https://incarnate.github.io/curl-to-php/)
- [דוקומנטציה של Guzzle](http://docs.guzzlephp.org/en/stable/)

המידע מוגש לרשותך, תתנהג לגביו באחריות.