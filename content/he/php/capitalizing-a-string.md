---
title:                "הגדלת אותיות במחרוזת"
date:                  2024-02-03T19:06:26.170118-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
הפיכת מחרוזת לאותיות ראשיות כוללת שינוי של התו הראשון של טקסט נתון לאות רישית, ודאי שמשפטים, כותרות או שמות עצמיים מתחילים כהלכה במערך נתונים. מתכנתים לעיתים קרובות מבצעים הפיכת מחרוזת לאותיות ראשיות על מנת לנרמל נתונים, לשפר קריאות או להבטיח עקביות בקלט משתמש או בעיבוד נתוני טקסט.

## איך לבצע:
PHP תומכת באופן טבעי במגוון פונקציות להפוך מחרוזות לאותיות ראשיות, כל אחת משרתת מטרה שונה. זהו הדרך שבה ניתן להשתמש בהן:

### הפיכת האות הראשונה של מחרוזת לאות רישית:

```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // מוצג: Hello, world!
```

### הפיכת האות הראשונה של כל מילה לאות רישית:

```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // מוצג: Hello, World!
```

### המרת כל המחרוזת לאותיות גדולות:

```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // מוצג: HELLO, WORLD!
```

לתרחישים הדורשים התאמה ספציפית יותר או פתרונות של צד שלישי, ניתן להשתמש בספריות כמו `mbstring` (למחרוזות מולטיבייט) במיוחד כאשר עוסקים בבינלאום שבו התווים עשויים להתפרש מעבר לסט ה-ASCII הבסיסי.

### שימוש ב-mbstring להפיכת מחרוזות UTF-8 לאותיות ראשיות:

ודאו שהרחבת `mbstring` מופעלת בהגדרות ה-PHP שלכם, לאחר מכן:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // מוצג: Élégant
```

הגישה זו עוזרת להפוך מחרוזות שכוללות תווים שאינם ASCII באופן מדויק, תוך הקפדה על גווני השפה השונים.
