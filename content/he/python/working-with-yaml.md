---
title:                "עבודה עם YAML"
date:                  2024-02-03T19:27:28.650344-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
YAML, שעומד ל-YAML Ain't Markup Language, הוא פורמט שיריאליזציה של נתונים קריא לאדם. תכנתים משתמשים ב-YAML עבור קובצי קונפיגורציה, הודעות בין-תהליכים, ואחסון נתונים בזכות התחביר הפשוט והקריאות הקלה לעומת פורמטים אחרים כמו XML או JSON.

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
