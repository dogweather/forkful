---
title:                "Python: עבודה עם פורמט json"
simple_title:         "עבודה עם פורמט json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-json.md"
---

{{< edit_this_page >}}

## למה
JSON הוא פורמט נתונים נפוץ ופופולרי בתכנות המיועד להעברת נתונים מבנה אחד למבנה אחר. עבודה עם נתוני JSON יכולה לקלוט ולפענח נתונים מורכבים בקלות ומהירות, מה שהופך אותו לכלי חשוב להעברת מידע בין שפות תכנות או כעמוד נתונים באפליקציות מגוונות.

## איך לעבוד עם JSON בפייתון
כדי להתחיל לעבוד עם נתוני JSON בפייתון, ניתן להשתמש בספריית המובנית json. בפנים, ניתן לשתמש בפקודות פייתון כדי לקרוא ולכתוב נתוני JSON ולפענח אותם בקלות. נהדר לתחיל להכיר את פקודת loads כדי לקרוא נתונים מקובץ JSON ולהמיר אותם למבנה נתונים בפייתון, ואת פקודת dumps כדי להמיר מבנה נתונים בפייתון לנתוני JSON ולכתוב אותם לקובץ.

````python
import json

# קריאת קובץ JSON
with open('data.json') as file:
    data = json.load(file)

# כתיבת נתוני JSON לקובץ
dict_data = {'name': 'John', 'age': 30}
with open('output.json', 'w') as file:
    json.dump(dict_data, file)
````

## למעמקים נוספים בפייתון עם JSON
נתוני JSON יכולים לכלול מבנים מורכבים ופרמטרים מסובכים. תיעוד מפורט של מודול json יכול לעזור לך ללמוד עוד על אופני קריאה וכתיבת נתונים מורכבים בפייתון. בנוסף, ניתן להשתמש בספריות נוספות כמו requests לקבלת נתוני JSON מאתרים חיצוניים, ולהשתמש בפונקציית eval כדי לפענח נתונים כמעט בכל הפורמטים בפייתון.

## ראה גם
- [תיעוד פייתון רשמי על מודול