---
title:                "לעבוד עם yaml"
html_title:           "Lua: לעבוד עם yaml"
simple_title:         "לעבוד עם yaml"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה? 
מטרת העבודה עם YAML היא לאפשר העברת מידע מורכב בין שפות תכנות שונות בצורה פשוטה וקלה. תוכניות נכתבות בשפות שונות, עבור תקשורת וגיבוי בקוד ויש יישומים מורכבים הדורשים שימוש בשפות שונות. YAML מספק דרך בטוחה וייעודית להעביר ולארגן מידע בין שפות. 

## איך לעשות: 
תחביר YAML דומה במרביתו ל-JSON, ולכן משתמשים בה כדי לכתוב מידע בצורה ברורה וקריאה. השפה פונקציונאלית, ולכן ניתן לעבוד איתה בכמה צורות. 
כאן ניתן לראות דוגמה פשוטה של כתיבת קוד ב-Lua עם פונקציית YAML כדי לארגן מידע: 
```
--טעינת הספריות הדרושות
local yaml = require "yaml" 
--יצירת מילון עם כמה שדות 
local dictionary = { 
  name = 'John', 
  age = 27, 
  hobbies = {'reading', 'painting', 'hiking'} 
} 
--המרת המילון לקובץ YAML
local yaml_text = yaml.dump(dictionary) 
--הדפסת התוצאה 
print(yaml_text) 
```
תוצאת התוכנה היא: 
```
name: John 
age: 27 
hobbies: 
- reading 
- painting 
- hiking
```

## טיול עמוק: 
YAML, שם הקצר ל-"YAML Ain't Markup Language", נוצר בשנת 2001 כדי להיות דרך נוחה יותר להעברת מידע מורכב מתוך XML. כיום הוא משמש כטכנולוגיה פופולרית יותר מאי פעם ומשמש להעברת מידע בין שפות תכנות, פתרונות ענן, סביבות אינטרנט ועוד. בין האלטרנטיבות ל-YAML ניתן למצוא את JSON (אותו יישום לשפת תכנות כמו YAML) ו-TOML (תומל), ששימש כתחליף עבור קבצים בפורמט ini. 
כאשר עובדים עם YAML, חשוב לזכור כי פני העבודה של השפה הם משתנים של מחרוזות פסקל ולא טיפוסים מוגדרים בשפת תכנות. כמו כן, חבילות פסקל יכולות להיות מעוצבות כדי להתקל בקוד יותר פשוט וקריא. 

## ראו גם: 
כאן ניתן למצוא מדריכים, דוגמאות נוספות, ותיעוד מידע: 
- אתר הבית של YAML: https://yaml.org/ 
- דוגמאות ותיעוד מידע נוספים עבור YAML במדריכי ה-W3Schools: https://www.w3schools.io/file-formats/yaml/