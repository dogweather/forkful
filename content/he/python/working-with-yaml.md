---
title:                "עבודה עם YAML"
html_title:           "Python: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

האם אתה חושב על עבודה עם YAML אבל לא חיות לך מספיק ידע על זה? בכתבה זו, אני אסביר לך את הסיבות למה משתמשים ב YAML ואיך ניתן להשתמש בו בקלות עם פייתון.

## איך להשתמש

כדי להתחיל לעבוד עם YAML תמיד עדיף ליצור קובץ חדש. השתמש בפקודה `yaml.dump()` כדי ליצור קובץ חדש עם הנתונים הרלוונטיים. לדוגמה, ניתן להשתמש בפקודה הבאה כדי ליצור קובץ YAML דרך קוד פייתון:

```Python
import yaml

# יוצר קבצים חדשים עם נתונים YAML ללא מנגנון spacing
yaml.dump(data, default_flow_style=False)

# יוצר קבצים חדשים עם נתונים YAML עם מנגנון spacing 
yaml.dump(data, default_flow_style=True)
```

ניתן להשתמש גם בפקודה `yaml.load()` כדי לקרוא קבצי YAML ולהחזיר אותם כסטרים בפייתון. לדוגמה, הנה קוד פייתון שמראה כיצד ניתן לקרוא ולהדפיס את נתוני YAML:

```Python
import yaml

# קריאה והדפסה של נתוני הקובץ YAML
with open('my_yaml_file.yml', 'r') as yaml_file:
    data = yaml.load(yaml_file, Loader=yaml.FullLoader)
    print(data)
```
**פלט**

```
{'name': 'John', 'age': 30, 'profession': 'developer'}
```

לדוגמה, אם נבקש מהמשתמש להכניס נתונים שונים לקובץ YAML ונרצה לקרוא את הנתונים שהוזנו על ידו, ניתן לעשות זאת בקלות עם הקוד הבא:

```Python
import yaml

# שומר את הנתונים הרלוונטיים מהמשתמש
name = input("הכנס את השם שלך: ")
age = int(input("הכנס את הגיל שלך: "))
profession = input("הכנס את המקצוע שלך: ")

# יוצר קובץ YAML עם הנתונים החדשים
data = {'name': name, 'age': age, 'profession': profession}
yaml.dump(data, open("my_yaml_file.yml", "w"))

# קריאה והדפסה של נתוני הקוב