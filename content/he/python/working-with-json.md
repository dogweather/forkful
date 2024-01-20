---
title:                "עבודה עם json"
html_title:           "Python: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-json.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
עבודה עם JSON היא תהליך בו מתבצעת יצירה, קריאה ועריכה של מידע מבוסס טקסט בפורמט JSON בקוד פייתון. הסיבה שפרוגרמים מפעילים פעולות על קבצי JSON היא שהם מייצגים דרך נוחה ופשוטה להעביר ולאחסן מידע בין שירותים ואפליקציות שונות.

## איך לעשות:
ניתן ליצור קובץ JSON חדש באמצעות הפקודה ```Python json.dump(data, file)``` על מנת לכתוב את המידע שבקובץ, ולקרוא ממנו באמצעות הפקודה ```Python json.load(file)```. לדוגמה, אם יש לנו קובץ מכיל את המידע הבא:
```
{
  "name": "John Smith",
  "age": 30,
  "email": "johnsmith@example.com"
}
```
אנחנו יכולים לקרוא אותו כדלקמן:
```Python
import json

with open("data.json") as file:
    data = json.load(file)
    
print(data["name"]) # מציג את הערך "John Smith"
```

## מעמקים:
פעולות עם JSON נעשו אפשריות בפייתון רק לאחר תוספת המודול json בגרסת 2.6.0 של השפה. קיימות גם אלטרנטיבות כגון YAML ו-XML, אך הפורמט הפשוט והקצר של JSON הופך אותו לעדיפות נכונה לרוב. בכדי לעבוד עם מבני JSON יש להשתמש בנתוני יסוד פייתוניים כמו רשימות ומילונים, ולהיזהר מטופסים חיצוניים כגון קבצי טקסט תמימים.

## ראה גם:
לקריאה נוספת על עבודה עם JSON בפייתון, הנה כמה מקורות מומלצים:
- [המדריך המפורט של W3Schools על JSON בפייתון](https://www.w3schools.com/python/python_json.asp)