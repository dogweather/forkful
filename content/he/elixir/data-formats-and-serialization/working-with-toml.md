---
title:                "עבודה עם TOML"
aliases:
- /he/elixir/working-with-toml/
date:                  2024-01-26T04:21:06.378675-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/working-with-toml.md"
---

{{< edit_this_page >}}

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
