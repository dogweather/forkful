---
date: 2024-01-26 04:21:06.378675-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML \u05E4\u05D9\u05E8\u05D5\
  \u05E9\u05D4 \u05DC\u05E0\u05EA\u05D7 \u05D5\u05DC\u05D9\u05D9\u05E6\u05E8 \u05E0\
  \u05EA\u05D5\u05E0\u05D9 TOML (Tom's Obvious, Minimal Language) \u05D1\u05D0\u05DE\
  \u05E6\u05E2\u05D5\u05EA Elixir. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D5 \u05DB\u05D3\u05D9 \u05DC\u05D4\
  \u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 \u05EA\u05E6\
  \u05D5\u05E8\u05D4 \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9-TOML\u2026"
lastmod: '2024-03-13T22:44:38.812360-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML \u05E4\u05D9\u05E8\u05D5\
  \u05E9\u05D4 \u05DC\u05E0\u05EA\u05D7 \u05D5\u05DC\u05D9\u05D9\u05E6\u05E8 \u05E0\
  \u05EA\u05D5\u05E0\u05D9 TOML (Tom's Obvious, Minimal Language) \u05D1\u05D0\u05DE\
  \u05E6\u05E2\u05D5\u05EA Elixir."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
weight: 39
---

## מה ולמה?
עבודה עם TOML פירושה לנתח ולייצר נתוני TOML (Tom's Obvious, Minimal Language) באמצעות Elixir. מתכנתים משתמשים בו כדי להתמודד עם קבצי תצורה מכיוון ש-TOML קריא, קל לניתוח וממופה היטב אל מבנה נתונים האש.

## איך לעשות:
ראשית, הוסף מנתח TOML לתלות של מיקס. הדוגמא הזו משתמשת ב-`toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

קרא קובץ TOML:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

כדי להמיר נתוני Elixir ל-TOML:

```elixir
data = %{title: "TOML Example", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

דוגמא לפלט:

```elixir
"title = \"TOML Example\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## צלילה עמוקה
TOML נוצר על ידי Tom Preston-Werner, שותף מייסד של GitHub, לשימוש בקבצי תצורה. הוא תוכנן להיות יותר פשוט מ-XLM ויותר תמציתי מ-YAML תוך שמירה על עקביות.

בין האלטרנטיבות נמנים קבצי JSON, YAML ו-INI, כל אחד עם התפשרויות שלו בקריאות אנושית ותאימות מבנה נתונים. TOML מצטיין בהצגה ברורה של נתונים טבלאיים וקיבוצים מקוננים של נתונים.

ב-Elixir, ההתמודדות עם TOML תלויה בספריות קידוד וניתוח, הממירות מחרוזות TOML למפות Elixir ולהפך. הניתוח עובד על ידי התאמה לכללי תחביר של TOML והמרתם לסוגי נתונים של Elixir. הקידוד עושה את ההפך על ידי מיפוי סוגי הנתונים של Elixir בחזרה לתחביר TOML תקף.

## ראה גם
- שפת TOML: https://toml.io/en/
- מאגר GitHub של `toml-elixir`: https://github.com/bitwalker/toml-elixir
- פרטי חבילת Hex עבור `toml-elixir`: https://hex.pm/packages/toml_elixir
