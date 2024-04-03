---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:16.991855-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05DC\u05DE\u05E8\u05D5\u05EA \u05E9-Fish Shell \u05E2\u05E6\u05DE\u05D5 \u05D0\
  \u05D9\u05E0\u05D5 \u05DB\u05D5\u05DC\u05DC \u05E4\u05E7\u05D5\u05D3\u05D4 \u05DE\
  \u05D5\u05D1\u05E0\u05D9\u05EA \u05DC-regex, \u05D4\u05D5\u05D0 \u05DE\u05E9\u05EA\
  \u05DE\u05E9 \u05D1\u05D9\u05E2\u05D9\u05DC\u05D5\u05EA \u05D1\u05E4\u05E7\u05D5\
  \u05D3\u05D5\u05EA \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05D5\u05EA \u05DB\u05DE\
  \u05D5 `grep`, `sed` \u05D5-`awk` \u05E9\u05EA\u05D5\u05DE\u05DB\u05D5\u05EA \u05D1\
  -regex, \u05DE\u05D4\u2026"
lastmod: '2024-03-13T22:44:40.028330-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05DE\u05E8\u05D5\u05EA \u05E9-Fish Shell \u05E2\u05E6\u05DE\u05D5\
  \ \u05D0\u05D9\u05E0\u05D5 \u05DB\u05D5\u05DC\u05DC \u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC-regex, \u05D4\u05D5\u05D0 \u05DE\u05E9\
  \u05EA\u05DE\u05E9 \u05D1\u05D9\u05E2\u05D9\u05DC\u05D5\u05EA \u05D1\u05E4\u05E7\
  \u05D5\u05D3\u05D5\u05EA \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05D5\u05EA \u05DB\
  \u05DE\u05D5 `grep`, `sed` \u05D5-`awk` \u05E9\u05EA\u05D5\u05DE\u05DB\u05D5\u05EA\
  \ \u05D1-regex, \u05DE\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DA \u05DC\
  \u05DB\u05DC\u05D5\u05DC \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA regex \u05D1\u05E1\
  \u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05E9\u05DC\u05DA."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
weight: 11
---

## איך לעשות זאת:
למרות ש-Fish Shell עצמו אינו כולל פקודה מובנית ל-regex, הוא משתמש ביעילות בפקודות חיצוניות כמו `grep`, `sed` ו-`awk` שתומכות ב-regex, מה שמאפשר לך לכלול פעולות regex בסקריפטים שלך.

### התאמת תבנית בסיסית עם `grep`
חיפוש שורות בקובץ שמתאימות לתבנית:

```fish
grep '^[0-9]+' myfile.txt
```

פקודה זו מוצאת שורות שמתחילות בספרה אחת או יותר ב-`myfile.txt`.

### חילוץ והחלפה עם `sed`
חילוץ מספרי טלפון מקובץ:

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

החלפת כל המופעים של "foo" ב-"bar" ב-`data.txt`:

```fish
sed 's/foo/bar/g' data.txt
```

### שימוש ב-`string` לביטויים רגולריים בסיסיים
פקודת `string` של Fish Shell תומכת בפעולות regex פשוטות כמו התאמה והחלפה:

התאמת תבנית במחרוזת:

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
פלט:
```
3.1.2
```

החלפת ספרות שלאחר 'fish' ב-'X.X.X':

```fish
echo "Welcome to fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
פלט:
```
Welcome to fish X.X.X
```

### התאמה מתקדמת עם `awk`
הדפסת העמודה השנייה של נתונים בה העמודה הראשונה מתאימה לתבנית מסוימת:

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

פקודה זו מחפשת שורות ב-`datafile` שבהן העמודה הראשונה מתחילה ב-"a" אחריו ספרה אחת או יותר ומדפיסה את העמודה השנייה.

על ידי שילוב פקודות חיצוניות אלו, תכנתי Fish Shell יכולים לנצל את הכוח המלא של ביטויים רגולריים למשימות עיבוד טקסט מורכבות, מה שמשדרג את היכולות המקוריות של המעטפת.
