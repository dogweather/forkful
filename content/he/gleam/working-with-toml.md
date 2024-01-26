---
title:                "עבודה עם TOML"
date:                  2024-01-26T04:22:45.706577-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/working-with-toml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם TOML פירושה לנתח ולייצר קבצי TOML (Tom's Obvious, Minimal Language) באמצעות קוד. מתכנתים משתמשים ב-TOML עבור קבצי תצורה קריאים וקלים לשימוש וגם לנרמול נתונים, בזכות הסמנטיקה הברורה שלו והתאימות שלו עם סוגי נתונים מסורתיים.

## איך לעשות:
בGleam אין תמיכה מובנית ב-TOML, ולכן תצטרך להשתמש בספרייה חיצונית. לדוגמה:

```gleam
// בהנחה שיש לך ספרייה לניתוח TOML:
import toml/{Parser, Encoder}

// ניתוח תוכן TOML
let toml_content = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

let parsed = Parser.parse(toml_content)

// שימוש בנתונים שננתחו
match parsed {
  Ok(data) -> "נתונים ננתחו בהצלחה!"
  Error(_) -> "ניתוח הנתונים נכשל."
}

// ייצור תוכן TOML ממבנה נתונים של Gleam
let data = #{
  "owner": #{
    "name": "Tom Preston-Werner",
    "dob": "1979-05-27T07:32:00Z"
  }
}

let toml_string = Encoder.encode(data)
```

פלט לדוגמה:

```
נתונים ננתחו בהצלחה!
```

## צלילה עמוקה
TOML שוחרר ב-2013 על ידי Tom Preston-Werner. המטרה שלו: להיות יותר קריא וישיר מ-XML ופחות מורכב מ-YAML עבור קבצי תצורה. למרות הפשטות, הוא עדיין עמיד עבור נתונים מובנים, ומציע תחביר מפורש וקל להבנה. חלופות כוללות JSON, YAML, ו-INI, אך התחביר המינימליסטי והברור של TOML לעיתים קרובות זוכה להעדפה עבור קבצי תצורה. הטמעת TOML ב-Gleam כוללת שני פעולות עיקריות: ניתוח TOML למבני נתונים מקומיים וסידור מבני נתונים מקומיים ל-TOML. רוב ספריות TOML עבור Erlang או Elixir יכולות להיות משולבות ב-Gleam בזכות התאימות שלו עם שפות BEAM, מה שמבטיח אינטגרציה חלקלקה בפרויקטים של Gleam.

## ראה גם
- מפרט השפה TOML: [https://toml.io/en/](https://toml.io/en/)
- מנתח TOML ב-Erlang: [https://hex.pm/packages/toml](https://hex.pm/packages/toml)
- TOML ב-GitHub: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)