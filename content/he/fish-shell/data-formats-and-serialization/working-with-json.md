---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:12.956106-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON \u05D1-Fish Shell \u05DB\
  \u05E8\u05D5\u05DB\u05D4 \u05D1\u05E0\u05D9\u05EA\u05D5\u05D7 \u05D5\u05D9\u05D9\
  \u05E6\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9 JSON, \u05DE\u05E9\u05D9\u05DE\
  \u05D4 \u05E0\u05E4\u05D5\u05E6\u05D4 \u05DC\u05E7\u05D1\u05D9\u05E2\u05EA \u05EA\
  \u05E6\u05D5\u05E8\u05D4 \u05E9\u05DC \u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\u05DD\
  , \u05EA\u05E7\u05E9\u05D5\u05E8\u05EA API, \u05D5\u05E9\u05D9\u05E4\u05D5\u05E8\
  \ \u05D6\u05E8\u05D9\u05DE\u05D5\u05EA \u05E2\u05D1\u05D5\u05D3\u05D4 \u05D1\u05E9\
  \u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4. \u05D1\u05D4\u05EA\u05D7\
  \u05E9\u05D1\u2026"
lastmod: 2024-02-19 22:04:59.362008
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON \u05D1-Fish Shell \u05DB\
  \u05E8\u05D5\u05DB\u05D4 \u05D1\u05E0\u05D9\u05EA\u05D5\u05D7 \u05D5\u05D9\u05D9\
  \u05E6\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9 JSON, \u05DE\u05E9\u05D9\u05DE\
  \u05D4 \u05E0\u05E4\u05D5\u05E6\u05D4 \u05DC\u05E7\u05D1\u05D9\u05E2\u05EA \u05EA\
  \u05E6\u05D5\u05E8\u05D4 \u05E9\u05DC \u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\u05DD\
  , \u05EA\u05E7\u05E9\u05D5\u05E8\u05EA API, \u05D5\u05E9\u05D9\u05E4\u05D5\u05E8\
  \ \u05D6\u05E8\u05D9\u05DE\u05D5\u05EA \u05E2\u05D1\u05D5\u05D3\u05D4 \u05D1\u05E9\
  \u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4. \u05D1\u05D4\u05EA\u05D7\
  \u05E9\u05D1\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם JSON ב-Fish Shell כרוכה בניתוח וייצור נתוני JSON, משימה נפוצה לקביעת תצורה של יישומים, תקשורת API, ושיפור זרימות עבודה בשורת הפקודה. בהתחשב בנוכחות הכללית של JSON בפיתוח אתרים ויישומים, שליטה בהפעלתו ישירות בשל יכולה לשפר משמעותית את היעילות באוטומציה ובטיפול בנתונים עבור מתכנתים.

## איך לעשות:

Fish Shell, בפני עצמו, אינו מכיל כלים מובנים לניתוח וייצור JSON. עם זאת, הוא משתלב בחלקה עם כלים חיצוניים כמו `jq` לעיבוד JSON. `jq` הוא מעבד JSON בשורת הפקודה, חזק וגמיש, המאפשר לך לחתוך, לסנן, למפות, ולשנות נתונים מובנים בעזרת שפה פשוטה וביטוייתית.

### ניתוח JSON עם jq
לנתח קובץ JSON ולחלץ ממנו נתונים באמצעות `jq`:
```fish
# בהנחה ויש לך קובץ JSON בשם 'data.json' עם התוכן: {"name":"Fish Shell","version":"3.4.0"}
cat data.json | jq '.name'
# דוגמא לפלט
"Fish Shell"
```

### יצירת JSON עם jq
יצירת תוכן JSON ממשתנים או פלטים בשל:
```fish
# יצירת אובייקט JSON ממשתנים
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# דוגמא לפלט
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### סינון אוספי JSON
נניח שיש לנו מערך של אובייקטים ב-JSON בקובץ בשם `versions.json`:
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
לסנן את המערך הזה לגרסאות יציבות בלבד:
```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# דוגמא לפלט
"3.1.2"
"3.4.0"
```

הדוגמאות הנתונות מדגימות את העוצמה שבאינטגרציה של `jq` עם Fish Shell לטיפול ב-JSON. השימוש בכלים כאלו מעשיר את חוויית השל, הופך אותו לסביבה חזקה לטיפול בפורמטים מודרניים של נתונים.
