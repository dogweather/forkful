---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:32.578686-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Elixir \u05DC\u05D0\
  \ \u05DB\u05D5\u05DC\u05DC\u05EA \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\
  \u05E0\u05D9\u05EA \u05D1-YAML. \u05E2\u05DD \u05D6\u05D0\u05EA, \u05E0\u05D9\u05EA\
  \u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\
  \u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DB\u05D2\u05D5\u05DF `yamerl`\
  \ \u05D0\u05D5 `yaml_elixir` \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\
  \u05DD YAML. \u05DB\u05D0\u05DF, \u05E0\u05EA\u05DE\u05E7\u05D3\u2026"
lastmod: '2024-03-13T22:44:38.807468-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u05DC\u05D0 \u05DB\u05D5\u05DC\u05DC\u05EA \u05EA\u05DE\u05D9\u05DB\
  \u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05D1-YAML."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## איך לעשות:
Elixir לא כוללת תמיכה מובנית ב-YAML. עם זאת, ניתן להשתמש בספריות צד שלישי כגון `yamerl` או `yaml_elixir` כדי לעבוד עם YAML. כאן, נתמקד ב-`yaml_elixir` עקב קלות השימוש שלה והתכונות המקיפות.

ראשית, הוסף את `yaml_elixir` לתלות mix.exs שלך:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

לאחר מכן, הפעל `mix deps.get` כדי לגרוף את התלות החדשה.

### קריאת YAML
נתון קובץ YAML פשוט, `config.yaml`, שנראה כך:

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

ניתן לקרוא את קובץ YAML זה ולהמיר אותו למפת Elixir כך:

```elixir
defmodule Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# דוגמא לשימוש
Config.read()
# פלט: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### כתיבת YAML
כדי לכתוב מפה חזרה לקובץ YAML:

```elixir
defmodule ConfigWriter do
  def write do
    content = %{
      database: %{
        adapter: "mysql",
        username: "root",
        password: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", content)
  end
end

# דוגמא לשימוש
ConfigWriter.write()
# זה ייצור או יחליף את `new_config.yaml` עם התוכן המצוין
```

שימו לב כיצד `yaml_elixir` מאפשרת תרגום ישיר בין קבצי YAML למבני נתונים של Elixir, והופכת אותה לבחירה מצוינת עבור מתכנתי Elixir שצריכים לעבוד עם נתוני YAML.
