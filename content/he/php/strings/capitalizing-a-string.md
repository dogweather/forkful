---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:26.170118-07:00
description: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05D5\u05EA\
  \ \u05DB\u05D5\u05DC\u05DC\u05EA \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05D4\
  \u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF \u05E9\u05DC \u05D8\u05E7\u05E1\
  \u05D8 \u05E0\u05EA\u05D5\u05DF \u05DC\u05D0\u05D5\u05EA \u05E8\u05D9\u05E9\u05D9\
  \u05EA, \u05D5\u05D3\u05D0\u05D9 \u05E9\u05DE\u05E9\u05E4\u05D8\u05D9\u05DD, \u05DB\
  \u05D5\u05EA\u05E8\u05D5\u05EA \u05D0\u05D5 \u05E9\u05DE\u05D5\u05EA \u05E2\u05E6\
  \u05DE\u05D9\u05D9\u05DD \u05DE\u05EA\u05D7\u05D9\u05DC\u05D9\u05DD \u05DB\u05D4\
  \u05DC\u05DB\u05D4 \u05D1\u05DE\u05E2\u05E8\u05DA \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
lastmod: 2024-02-19 22:04:58.694347
model: gpt-4-0125-preview
summary: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05D5\u05EA \u05DB\
  \u05D5\u05DC\u05DC\u05EA \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05D4\u05EA\
  \u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF \u05E9\u05DC \u05D8\u05E7\u05E1\u05D8\
  \ \u05E0\u05EA\u05D5\u05DF \u05DC\u05D0\u05D5\u05EA \u05E8\u05D9\u05E9\u05D9\u05EA\
  , \u05D5\u05D3\u05D0\u05D9 \u05E9\u05DE\u05E9\u05E4\u05D8\u05D9\u05DD, \u05DB\u05D5\
  \u05EA\u05E8\u05D5\u05EA \u05D0\u05D5 \u05E9\u05DE\u05D5\u05EA \u05E2\u05E6\u05DE\
  \u05D9\u05D9\u05DD \u05DE\u05EA\u05D7\u05D9\u05DC\u05D9\u05DD \u05DB\u05D4\u05DC\
  \u05DB\u05D4 \u05D1\u05DE\u05E2\u05E8\u05DA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
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
