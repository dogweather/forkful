---
title:                "Bash: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/working-with-json.md"
---

{{< edit_this_page >}}

## למה

ניתן להשתמש בפורמט JSON בתכנות בש כדי לאחסן ולשלוח נתונים בין אפליקציות ושירותים. פורמט זה מאפשר לנתונים להיות קריאים ונגישים לשפות תכנות מגוונות ומומלץ לשימוש במגוון רחב של פרויקטים.

## איך לבצע

ניתן ליצור, לקרוא ולערוך קבצי JSON בעזרת כלי סופרים פופולריים כמו `jq` ו-`python-json`. בנוסף, באפשרותך לשלב הפורמט בתכניות כדי לבצע בדיקות משובחות יותר ולנהל מבני נתונים מורכבים יותר.

```Bash
# יצירת קובץ JSON ריק
touch data.json

# הוספת נתונים לקובץ בעזרת jq
echo '{"name": "יעקב", "age": 32}' | jq >> data.json

# קריאת נתונים מקובץ והצגתם בעזרת jq
jq . data.json
# פלט:
# {
#    "name": "יעקב",
#    "age": 32
# }

# עריכת נתונים בקובץ עם python-json
python -m json.tool sample.json
# פלט:
# {
#    "name": "יעקב",
#    "age": 33
# }
```

## חפירה עמוקה

כדי לשלת בצורה מבונהת ומסודרת בין אפליקציות ושירותים, כדאי להכיר את הידע בסיסי לגבי סטנדרט JSON Schema. הסכמה זו מאפשרת להגדיר ולבצע בדיקות על הנתונים, ולוודא שהם תואמים למבנה המוגדר מראש.

## ראה גם

 - [JSON ב-Wikipedia](https://en.wikipedia.org/wiki/JSON)
 - [מדריך תחזוקה לפורמט JSON](https://www.json.org/js.html)
 - [מסמכי JSON Schema](https://json-schema.org/)