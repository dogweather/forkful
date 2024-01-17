---
title:                "עובדים עם YAML"
html_title:           "Python: עובדים עם YAML"
simple_title:         "עובדים עם YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-yaml.md"
---

{{< edit_this_page >}}

# מה ולמה?
עבודה עם YAML היא תהליך נפוץ בתכנות שמאפשר למפענחים ולמתכנתים להפעיל וליצור נתונים מסוגים שונים בקובץ נקרא YAML. מספר רב של תכנים כמו בניית קונפיגורציות ותיעוד מבוסס YAML ויכולותיו הגדולות כשמדובר בכתיבת קצרה, עושים עבודה עם קבצי YAML חשובה מאוד למפענחים בשפת Python.

# איך ל:

הנה דוגמאות של קוד Python כדי להציג התנהגות של קבצים YAML:

```
# ייבוא ספריות הנחוצות
from yaml import load, dump  

# קבצי ימאל ניתנים לקריאה עם פקודה הבאה
with open("example.yaml", 'r') as stream:
    yaml_data = load(stream)

# כדי לכתוב קבצי YAML ניתן להשתמש בפקודה הבאה
with open("output.yaml", 'w') as out:
    yaml_data = dump(yaml_data, out)
```

# מחקר מעמיק

### היסטוריית תאימות

נוסד בשנת 2001 על ידי שתי חברות טכנולוגיות גרמניות, YAML היום מחרשיות עם פופולריות ותמיכה רחבה בכמה שפות תכנות כמו Python, C # ו- Java. כיום, YAML משמש כקובץ המכיל מידע תדינמי ויכולת הכתיבה הפשוטה שלו הופכת אותו לאטרקטיבי במיוחד לתכנות פייתון.

### אלטרנטיבות

במקום לעבוד עם YAML, אפשר גם להשתמש בפורמטים אחרים כמו JSON או XML. גם אם ישנן תכונות חדשות כמו JSONB ו- JSONS, YAML עדיין נחשב לאחד הפורמטים המוצלחים ביותר בתכנות.

### פיתוח עם YAML בפייתון

כדי להתאים ליישומים בפייתון יותר קשרי YAML ניתן להשתמש בספריית PyYAML, השומרת על קוד בטוח כשמדובר במרחב של פרוטוקולי הקוד בתכנון מערכות.

# ראו גם

* [קובץ YAML הרשמי](https://yaml.org/)
* [תיעוד YAML עבור שפת Python](https://pyyaml.org/wiki/PyYAMLDocumentation)