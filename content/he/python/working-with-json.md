---
title:                "עבודה עם JSON"
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON הוא תסדיר להחלפת נתונים, קליל ונקרא בקלות על ידי בני אדם ומחולל בקלות על ידי מחשבים. תוכניתנים עובדים עם JSON כדי לשלוח ולקבל נתונים באפליקציות ווב, API-ים ועוד.

## איך לעשות:
טעינת JSON מקובץ:
```Python
import json

with open('data.json', 'r', encoding='utf-8') as f:
    data = json.load(f)
print(data)
```

יצירת JSON ממילון פייתון:
```Python
import json

data = {"name": "משה", "age": 30, "city": "תל אביב"}
json_data = json.dumps(data, ensure_ascii=False)
print(json_data)
```

פלט לדוגמה:
```Python
{"name": "משה", "age": 30, "city": "תל אביב"}
```

## עומק ידע:
JSON, שמהווה קיצור של JavaScript Object Notation, פותח לראשונה כתחליף ל-XML. כיום הפורמט הוא סטנדרט אינטרנטי שנמצא בשימוש נרחב. קיימות אלטרנטיבות כמו YAML או XML, אך JSON הוא מקובל יותר בגלל פשטותו. JSON אינו תלוי שפה, אך הוא מיושם בקלות בפייתון באמצעות המודול `json` המובנה.

## ראו גם:
- [תיעוד JSON של פייתון](https://docs.python.org/3/library/json.html)
- [מבוא ל-JSON](https://www.json.org/json-en.html)
- [הבדלים בין JSON ל-XML](https://www.w3schools.com/js/js_json_xml.asp)
- [RFC 7159 - מפרט ה-JSON הרשמי](https://tools.ietf.org/html/rfc7159)