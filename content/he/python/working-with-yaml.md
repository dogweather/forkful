---
title:                "עבודה עם YAML"
date:                  2024-01-19
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט המרה של נתונים, נוח לקריאה וכתיבה אנושית. תכניתנים משתמשים בו כדי לקבוע קונפיגורציה או לשמש כפורמט החלפה בין שונות מערכות.

## איך לעשות:
קודם כל, התקנו את חבילת PyYAML:
```Python
pip install PyYAML
```

קריאת קובץ YAML:
```Python
import yaml

with open('config.yaml', 'r') as stream:
    try:
        data = yaml.safe_load(stream)
        print(data)
    except yaml.YAMLError as exc:
        print(exc)
```

כתיבת קובץ YAML:
```Python
import yaml

data = {'database': {'user': 'root', 'password': 'example'}}

with open('config.yaml', 'w') as file:
    yaml.dump(data, file, default_flow_style=False)
```

אם תשמרו ותריצו את הקוד הזה, תקבלו קובץ `config.yaml` המכיל את הנתונים שהוגדרו בתוך `data`.

## צלילה עמוקה
YAML, שם מקוצר של "YAML Ain't Markup Language" (המתחיל בצורה רקורסיבית), נוצר באופן פורמלי ב-2001. אלטרנטיבות פופולריות כוללות JSON ו-XML. ב-YAML, רווחים והיכול לציין קשרי היררכיה, וזה חשוב להבין כאשר מבצעים parse לקובץ.

## ראו גם
- מסמכי PyYAML הרשמיים: https://pyyaml.org/wiki/PyYAMLDocumentation
- מבוא ל-YAML: https://gettaurus.org/docs/YAMLTutorial/
- Stack Overflow לשאלות ותשובות על YAML בפייתון: https://stackoverflow.com/questions/tagged/yaml+python
