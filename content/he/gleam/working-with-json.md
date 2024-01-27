---
title:                "עבודה עם JSON"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON, פורמט להעברת נתונים, פופולרי ביותר. מתכנתים משתמשים בו מכיוון שהוא פשוט, קל לקריאה, ומתממשק בקלות עם שפות תכנות רבות.

## How to:
כדי לעבוד עם JSON ב-Gleam, קודם תצטרכו להתקין את החבילה `gleam_json`. הנה דוגמא לקוד:

```gleam
import gleam/json
import gleam/map

pub fn main() {
  let data = map.from_list([
    ("name", "Dana"),
    ("age", 32),
  ])
  let json = json.from(map.to_dynamic(data))
  json.encode() // מפורמט ל-JSON
}
```
פלט דוגמה:
```json
{"name": "Dana", "age": 32}
```

## Deep Dive
JSON (JavaScript Object Notation) נוצר בשנת 2001. האלטרנטיבות כוללות XML ו-YAML, אבל JSON נשאר האופציה המועדפת בזכות פשטותו. כשעובדים עם Gleam, הספריה `gleam_json` מתרגמת מבנים של Gleam ל-JSON ולהפך באופן אוטומטי.

## See Also
- מדריך Gleam: https://gleam.run/book/
- מסמכים של `gleam_json`: https://hexdocs.pm/gleam_json/
- הכירו JSON: https://www.json.org/json-en.html
