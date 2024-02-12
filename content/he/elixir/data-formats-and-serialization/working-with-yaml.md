---
title:                "עבודה עם YAML"
date:                  2024-02-03T19:25:32.578686-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

YAML, שמקוצר מ-YAML Ain't Markup Language, הוא תקן לסידור נתונים קריא לאדם ונפוץ בעיקר לשימוש בקבצי תצורה והחלפת נתונים בין שפות עם מבני נתונים שונים. מתכנתים משתמשים בו בגלל הפשטות שלו ויכולתו לייצג בקלות נתונים היררכיים מורכבים.

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
