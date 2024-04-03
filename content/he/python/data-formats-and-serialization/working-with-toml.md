---
date: 2024-01-26 04:26:07.619347-07:00
description: "TOML, \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\u05EA \u05E9\
  \u05DC Tom's Obvious, Minimal Language, \u05D4\u05D9\u05D0 \u05E4\u05D5\u05E8\u05DE\
  \u05D8 \u05DC\u05D0\u05E8\u05DB\u05D5\u05D1 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D4\u05D3\u05D5\u05DE\u05D4 \u05DC-JSON \u05D0\u05D5 YAML, \u05D0\u05DA \u05E9\
  \u05D5\u05D0\u05E3 \u05DC\u05E4\u05E9\u05D8\u05D5\u05EA \u05D5\u05DC\u05E7\u05E8\
  \u05D9\u05D0\u05D5\u05EA. \u05EA\u05D5\u05DB\u05E0\u05D9\u05DD \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1-TOML \u05E2\u05D1\u05D5\u05E8\u2026"
lastmod: '2024-03-13T22:44:38.673865-06:00'
model: gpt-4-0125-preview
summary: "TOML, \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\u05EA \u05E9\u05DC\
  \ Tom's Obvious, Minimal Language, \u05D4\u05D9\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8\
  \ \u05DC\u05D0\u05E8\u05DB\u05D5\u05D1 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D4\
  \u05D3\u05D5\u05DE\u05D4 \u05DC-JSON \u05D0\u05D5 YAML, \u05D0\u05DA \u05E9\u05D5\
  \u05D0\u05E3 \u05DC\u05E4\u05E9\u05D8\u05D5\u05EA \u05D5\u05DC\u05E7\u05E8\u05D9\
  \u05D0\u05D5\u05EA."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
weight: 39
---

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
