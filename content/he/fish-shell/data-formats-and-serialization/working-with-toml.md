---
date: 2024-01-26 04:21:58.662844-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D5\u05DC\
  \u05E2\u05D1\u05D3 TOML \u05D1-Fish, \u05D0\u05E4\u05E9\u05E8 \u05DC\u05D4\u05E9\
  \u05EA\u05DE\u05E9 \u05D1\u05DB\u05DC\u05D9 \u05DB\u05DE\u05D5 `yj`, \u05E9\u05D9\
  \u05DB\u05D5\u05DC \u05DC\u05D4\u05DE\u05D9\u05E8 TOML \u05DC-JSON. \u05D4\u05E0\
  \u05D4 \u05D0\u05D9\u05DA."
lastmod: '2024-03-13T22:44:40.088844-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05E7\u05E8\u05D5\u05D0 \u05D5\u05DC\u05E2\u05D1\u05D3 TOML \u05D1\
  -Fish, \u05D0\u05E4\u05E9\u05E8 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05DB\
  \u05DC\u05D9 \u05DB\u05DE\u05D5 `yj`, \u05E9\u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\
  \u05DE\u05D9\u05E8 TOML \u05DC-JSON."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
weight: 39
---

## איך ל:
לקרוא ולעבד TOML ב-Fish, אפשר להשתמש בכלי כמו `yj`, שיכול להמיר TOML ל-JSON. הנה איך:

```fish
# התקנת yj דרך Fisher
fisher install jorgebucaran/yj

# המרת TOML ל-JSON
echo 'title = "TOML Example"' | yj -tj

# פלט לדוגמא
{"title":"TOML Example"}
```

לכתוב TOML, פשוט מבצעים את התהליך ההפוך:

```fish
# המרת JSON ל-TOML
echo '{"title":"JSON Example"}' | yj -jt

# פלט לדוגמא
title = "JSON Example"
```

לעבודות כבדות, שקלו שימוש בכלי CLI מוקדש ל-TOML כמו `toml-cli`.

```fish
# התקנת toml-cli
pip install toml-cli

# הגדרת ערך בקובץ TOML
toml set pyproject.toml tool.poetry.version "1.1.4"

# קבלת ערך מקובץ TOML
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## צלילה עמוקה
TOML (Tom's Obvious, Minimal Language), שהוצג על ידי Tom Preston-Werner ב-2013, דומה ל-INI אבל עם מפרט מוגדר והיררכיית נתונים. JSON ו-YAML הם החלופות העיקריות, אך יש להם מסחר ומנה: JSON אינו ידידותי כלפי המשתמש כמו TOML, בעוד ש-YAML יותר מורכב. עיצוב TOML מצליח בסיטואציות שבהן קבצי הגדרות נתונים לעיתים קרובות לטיפול ידני, מאזן בין פשטות לבין ביטוייות. מבחינת יישום, פרשני TOML זמינים עבור רוב שפות התכנות, כולל TomlBombadil עבור Fish שיכול להשתלב ישירות בתסריטים שלכם.

## ראו גם
- המפרט הרשמי של TOML: https://toml.io
- `yj`, כלי להמרה בין TOML, JSON, YAML, ו-XML: https://github.com/jorgebucaran/yj
- `toml-cli`, כלי שורת פקודה עבור TOML: https://github.com/sdispater/toml-cli
