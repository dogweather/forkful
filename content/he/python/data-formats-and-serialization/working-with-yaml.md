---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:28.650344-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E7\u05E8\u05D9\
  \u05D0\u05D4 \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4 \u05E9\u05DC YAML \u05D1\u05E4\
  \u05D9\u05D9\u05EA\u05D5\u05DF \u05DB\u05E8\u05D5\u05DB\u05D4 \u05DC\u05E8\u05D5\
  \u05D1 \u05D1\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E9\u05DC \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9, \u05DB\u05D0\u05E9\
  \u05E8 `PyYAML` \u05D4\u05D9\u05D0 \u05D4\u05E4\u05D5\u05E4\u05D5\u05DC\u05E8\u05D9\
  \u05EA \u05D1\u05D9\u05D5\u05EA\u05E8. \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D7\
  \u05D9\u05DC, \u05EA\u05E6\u05D8\u05E8\u05DA \u05DC\u05D4\u05EA\u05E7\u05D9\u05DF\
  \ \u05D0\u05EA PyYAML \u05E2\u05DC\u2026"
lastmod: '2024-03-13T22:44:38.669160-06:00'
model: gpt-4-0125-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05D4 \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4 \u05E9\
  \u05DC YAML \u05D1\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05DB\u05E8\u05D5\u05DB\u05D4\
  \ \u05DC\u05E8\u05D5\u05D1 \u05D1\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05D4 \u05E9\u05DC \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9\
  , \u05DB\u05D0\u05E9\u05E8 `PyYAML` \u05D4\u05D9\u05D0 \u05D4\u05E4\u05D5\u05E4\u05D5\
  \u05DC\u05E8\u05D9\u05EA \u05D1\u05D9\u05D5\u05EA\u05E8."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## איך לעשות:
קריאה וכתיבה של YAML בפייתון כרוכה לרוב בשימוש בספרייה של צד שלישי, כאשר `PyYAML` היא הפופולרית ביותר. כדי להתחיל, תצטרך להתקין את PyYAML על ידי הרצת `pip install PyYAML`.

**דוגמא: כתיבה לקובץ YAML**

```python
import yaml

data = {'a list': [1, 42, 3.141, 1337, 'help', u'€'],
        'a string': 'boo!',
        'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}

with open('example.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# זה יוצר `example.yaml` עם הנתונים מובנים בפורמט YAML.
```

**דוגמא: קריאה מקובץ YAML**

```python
import yaml

with open('example.yaml', 'r') as f:
    data_loaded = yaml.safe_load(f)

print(data_loaded)

# פלט: 
# {'a list': [1, 42, 3.141, 1337, 'help', '€'],
#  'a string': 'boo!',
#  'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}
```

**שימוש ב-YAML לקונפיגורציות**

רבים מהתכנתים משתמשים ב-YAML כדי לנהל קונפיגורציות של אפליקציות. הנה דוגמא איך אפשר לבנות קובץ קונפיג ולקרוא אותו:

config.yaml:
```yaml
database:
  host: localhost
  port: 5432
  username: admin
  password: secret
```

קריאה של קובץ הקונפיג בפייתון:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['database']['host'])  # פלט: localhost
```

**טיפול במבנים מורכבים**

עבור מבנים מורכבים, PyYAML מאפשרת לך להגדיר אובייקטים פייתון מותאמים אישית. עם זאת, ודא שאתה מנהל את התהליך באופן בטוח על ידי שימוש ב-`safe_load` כדי למנוע הרצת פונקציות או אובייקטים שרירותיים.

```python
import yaml

# הגדרת אובייקט פייתון
class Example:
    def __init__(self, value):
        self.value = value

# בנאי מותאם
def constructor_example(loader, node):
    value = loader.construct_scalar(node)
    return Example(value)

# הוספת בנאי עבור התג "!example"
yaml.add_constructor('!example', constructor_example)

yaml_str = "!example 'data'"
loaded = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(loaded.value)  # פלט: data
```

בקטע קוד זה, `!example` הוא תג מותאם לייצוג אובייקט של `Example` עם הערך 'data' ממחרוזת YAML. טועני טעינה מותאמים כמו זה מרחיבים את הגמישות של PyYAML, מאפשרים עיבוד של מבני נתונים וסוגים מורכבים יותר.
