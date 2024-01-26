---
title:                "עבודה עם TOML"
date:                  2024-01-26T04:26:07.619347-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-toml.md"
---

{{< edit_this_page >}}

## מה ולמה?
TOML, ראשי תיבות של Tom's Obvious, Minimal Language, היא פורמט לארכוב נתונים הדומה ל-JSON או YAML, אך שואף לפשטות ולקריאות. תוכנים משתמשים ב-TOML עבור קובצי תצורה כיוון שהוא קל לכתיבה ולהבנה, והוא מתמפה בצורה נאה למבני נתונים בשפות תכנות כמו Python.

## איך ל:
לפני הכול, התקן את החבילה `toml` עם `pip install toml`. בואו ננתח קובץ TOML:

```python
import toml

# דוגמת תוכן TOML כמחרוזת
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # תאריכים מחלקה ראשונה

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# לנתח את המחרוזת TOML
parsed_toml = toml.loads(toml_string)

# גישה לנתונים
print(parsed_toml['owner']['name'])  # פלט: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # פלט: [8001, 8001, 8002]
```

## ירידה לעומק
TOML נוצר על ידי Tom Preston-Werner, אחד ממייסדי GitHub, כפורמט קובץ תצורה ידידותי יותר למשתמש. הוא מעוצב לתיאום ללא עמימות לטבלת האש ולהיות קל לניתוח על ידי מכונות.

בהשוואה ל-JSON, TOML קריא יותר עבור קובצי תצורה ותומך בהערות. YAML, אלטרנטיבה נוספת, יכולה להיות יותר קומפקטית, אך התלות שלה בהזחה ובעיות עדינות, כמו האיסור על שימוש בטאבים, עלולות להפיל אנשים.

לגבי פרטי היישום, ערכי TOML הם מסוגפים, שכוללים מחרוזות, מספרים שלמים, מספרים עשרוניים, בוליאנים, תאריכים ושעות, מערכים וטבלאות. הכל רגיש לרישיות. בנוסף, TOML תומך במחרוזות מרובות שורות ו, כפי שנכון לגרסה האחרונה, אף מאפשר מערכים עם סוגים הטרוגניים.

Python משתמש בספריית ה-`toml`, המשקפת את הספריות JSON ו-YAML מבחינת API. יש לכם את `toml.load` ו-`toml.loads` לקריאת TOML מקובץ או מחרוזת, בהתאמה, ואת `toml.dump` ו-`toml.dumps` לכתיבתו.

## ראה גם
- מאגר ה-GitHub הרשמי של TOML למפרטים: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- תיעוד ספריית ה-`toml` ב-Python: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- דוגמאות מהעולם האמיתי של TOML: קובצי תצורה למנהל החבילות של Rust `cargo` או כלי האריזה של Python `poetry`.