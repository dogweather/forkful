---
title:                "עבודה עם קבצי csv"
html_title:           "Fish Shell: עבודה עם קבצי csv"
simple_title:         "עבודה עם קבצי csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Why
כדורש פרפקטי לעבוד עם קבצי CSV שכן הם מאפשרים שמירה וטעינה של מידע בפורמט פשוט, נקי וקריא.

## How To
ראשית, נצרף את הקובץ לתוך משתנה:
```fish
set my_csv (csv.from_file "file_name.csv")
```
אם יש כותרות בקובץ, ניתן לקרוא אותן ככה:
```fish
set headers $my_csv[1]
```
להדפסת כתובות השורות הבאות בקובץ ניתן לעשות כך:
```fish
echo $my_csv[2:]
```
ניתן גם לקרוא ערכים בצורה אינדיבידואלית:
```fish
echo $my_csv[2][1] # מדפיס את הפריט בשורה השנייה ובטור הראשון
```

## Deep Dive
כדי לעבוד עם קבצי CSV בפיש, ניתן להשתמש במודול csv על ידי צירוף שלו עם הפקודה `source` ובהתאם להוראות המתאימות. ניתן לכתוב עצם תכנית שתעטוף את כל תהליך הקריאה וההדפסה כדי להפשיט את השימוש בו.

## See Also
- [Fish Shell הדרכה מתחילים](https://fishshell.com/docs/current/tutorial.html)
- [מדריך CSV לפיש](https://github.com/evanlucas/fish-csv)
- [מדריך CSV מלא](https://csvkit.readthedocs.io/en/latest/)