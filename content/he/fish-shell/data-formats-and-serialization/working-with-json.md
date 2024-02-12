---
title:                "עבודה עם JSON"
aliases: - /he/fish-shell/working-with-json.md
date:                  2024-02-03T19:23:12.956106-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
