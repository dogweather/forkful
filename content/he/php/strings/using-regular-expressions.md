---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:12.631231-07:00
description: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D1-PHP \u05D4\u05DD \u05D3\u05E4\u05D5\u05E1\u05D9\
  \u05DD \u05D4\u05DE\u05E9\u05DE\u05E9\u05D9\u05DD \u05DC\u05D4\u05EA\u05D0\u05DE\
  \u05EA \u05E9\u05D9\u05DC\u05D5\u05D1\u05D9 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA, \u05DE\u05D4 \u05E9\u05DE\u05D0\u05E4\
  \u05E9\u05E8 \u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA\
  \ \u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05D4 \u05DE\u05D5\
  \u05E8\u05DB\u05D1\u05D5\u05EA \u05D5\u05D0\u05D9\u05DE\u05D5\u05EA \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E0\
  \u05E6\u05DC\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.464863-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D1-PHP \u05D4\u05DD \u05D3\u05E4\u05D5\u05E1\u05D9\
  \u05DD \u05D4\u05DE\u05E9\u05DE\u05E9\u05D9\u05DD \u05DC\u05D4\u05EA\u05D0\u05DE\
  \u05EA \u05E9\u05D9\u05DC\u05D5\u05D1\u05D9 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA, \u05DE\u05D4 \u05E9\u05DE\u05D0\u05E4\
  \u05E9\u05E8 \u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA\
  \ \u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05D4 \u05DE\u05D5\
  \u05E8\u05DB\u05D1\u05D5\u05EA \u05D5\u05D0\u05D9\u05DE\u05D5\u05EA \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E0\
  \u05E6\u05DC\u05D9\u05DD\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

ביטויים רגולריים (regex) ב-PHP הם דפוסים המשמשים להתאמת שילובי תווים במחרוזות, מה שמאפשר ביצוע פעולות חיפוש והחלפה מורכבות ואימות נתונים. מתכנתים מנצלים את ה-regex בשל כוחם וגמישותם בניתוח טקסטים, אימות טפסים או שליפת נתונים מהאינטרנט, מה שהופך אותו לכלי בלתי נפרד בארסנל של המפתח.

## איך ל:

PHP תומכת בביטויים רגולריים דרך ספריית ה-PCRE (Perl Compatible Regular Expressions), המציעה ערכת פונקציות עשירה. הנה איך להשתמש בהם:

### תיאום דפוס:

כדי לבדוק אם דפוס קיים בתוך מחרוזת, משתמשים ב-`preg_match()`. הפונקציה הזו מחזירה 1 אם הדפוס נמצא במחרוזת ו-0 אם לא.

```php
if (preg_match("/\bweb\b/i", "PHP is a web scripting language")) {
    echo "A match was found.";
} else {
    echo "A match was not found.";
}
// פלט: A match was found.
```

### מציאת כל ההתאמות:

`preg_match_all()` משמשת כאשר יש צורך למצוא את כל המופעים של דפוס בתוך מחרוזת.

```php
$text = "cats and dogs";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// פלט: Array ( [0] => cats [1] => and [2] => dogs )
```

### החלפת טקסט:

כדי להחליף טקסט שמתאים לביטוי רגולרי, משתמשים ב-`preg_replace()`. זה עוצמתי במיוחד לעיצוב וניקוי נתונים.

```php
$originalText = "April 15, 2003";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// פלט: April1,2003
```

### פיצול מחרוזות:

ניתן לפצל מחרוזת למערך באמצעות `preg_split()`, תוך שימוש בדפוס כמפריד.

```php
$text = "PHP is, an extremely popular, scripting language";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// פלט: Array ( [0] => PHP is [1] => an extremely popular [2] => scripting language )
```

בנוסף, למשימות ודפוסים מורכבים יותר ב-regex, מסגרות וספריות כמו רכיב ה-`Finder` של Symfony או אוסף פונקציות העזר של Laravel עשויות להציע שכבת אבסטרקציה נוחה יותר. עם זאת, הבנה וניצול של פונקציות ה-PCRE המובנות של PHP קריטיות לעיבוד ואימות טקסטים ביעילות ישירות דרך סקריפטים של PHP.
