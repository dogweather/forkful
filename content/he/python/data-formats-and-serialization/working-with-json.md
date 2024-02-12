---
title:                "עבודה עם JSON"
aliases: - /he/python/working-with-json.md
date:                  2024-02-03T19:24:39.548047-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם JSON (תסדיר אובייקטים של JavaScript) כוללת ניתוח מחרוזות בתסדיר JSON לאובייקטים של פייתון ולהפך. זה קריטי לפיתוח אינטרנט ו-API מכיוון ש-JSON הוא לשון הקודש להחלפת נתונים בין שרתים ללקוחות.

## איך לעשות:

הספרייה המובנית `json` בפייתון מפשטת את התהליך של קידוד (המרת אובייקטים של פייתון ל-JSON) ופענוח (המרת JSON לאובייקטים של פייתון). הנה איך אפשר להשתמש בה:

### קידוד אובייקטים של פייתון ל-JSON:

```python
import json

data = {
    "name": "John Doe",
    "age": 30,
    "isEmployee": True,
    "addresses": [
        {"city": "New York", "zipCode": "10001"},
        {"city": "San Francisco", "zipCode": "94016"}
    ]
}

json_string = json.dumps(data, indent=4)
print(json_string)
```

**פלט:**

```json
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
```

### פענוח JSON לאובייקטים של פייתון:

```python
json_string = '''
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
'''

data = json.loads(json_string)
print(data)
```

**פלט:**

```python
{
    'name': 'John Doe', 
    'age': 30, 
    'isEmployee': True, 
    'addresses': [
        {'city': 'New York', 'zipCode': '10001'}, 
        {'city': 'San Francisco', 'zipCode': '94016'}
    ]
}
```

### עבודה עם ספריות צד שלישי:

לניהול מורכב של JSON, כגון אימות סכימה או ניתוח קבצי JSON ישירות מ-URLs, ספריות כמו `requests` לבקשות HTTP ו-`jsonschema` לאימות יכולות להיות שימושיות.

#### דוגמה עם `requests` לניתוח JSON מ-URL:

```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

קטע קוד זה מבצע איסוף של נתוני JSON מ-URL נתון וממיר אותם ישירות לאובייקט של פייתון.

#### שימוש ב-`jsonschema` לאימות JSON:

ראשית, התקנת הספרייה דרך pip:

```bash
pip install jsonschema
```

לאחר מכן, השתמש בה כך:

```python
from jsonschema import validate
import jsonschema

schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "number"},
        "isEmployee": {"type": "boolean"},
    },
    "required": ["name", "age", "isEmployee"]
}

# בהנחה ש-`data` הוא מילון שהתקבל מפענוח JSON
try:
    validate(instance=data, schema=schema)
    print("נתוני JSON תקפים.")
except jsonschema.exceptions.ValidationError as err:
    print("שגיאת אימות:", err)
```

דוגמה זו מאמתת את המילון שלך בפייתון (שהתקבל מנתוני JSON שנפענחו) מול סכימה מוגדרת מראש, בדוקה שהנתונים עומדים בצורות וסוגים הצפויים.
