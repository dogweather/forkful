---
title:                "Att Arbeta med YAML"
aliases:
- /sv/elixir/working-with-yaml.md
date:                  2024-02-03T19:25:22.332085-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att Arbeta med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

YAML, som står för YAML Ain't Markup Language, är en standard för serialisering av data som är läsbar för människor och som vanligtvis används för konfigurationsfiler och datadelning mellan språk med olika datastrukturer. Programmerare använder den på grund av dess enkelhet och dess förmåga att enkelt representera komplex hierarkisk data.

## Hur man gör:

Elixir inkluderar inte inbyggt stöd för YAML. Du kan dock använda tredjepartsbibliotek såsom `yamerl` eller `yaml_elixir` för att arbeta med YAML. Här kommer vi att fokusera på `yaml_elixir` på grund av dess användarvänlighet och omfattande funktioner.

Först, lägg till `yaml_elixir` i dina beroenden i mix.exs:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

Kör sedan `mix deps.get` för att hämta det nya beroendet.

### Läsa YAML

Givet en enkel YAML-fil, `config.yaml`, som ser ut så här:

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

Du kan läsa denna YAML-fil och konvertera den till en Elixir-mapp på följande sätt:

```elixir
%module Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# Exempelanvändning
Config.read()
# Output: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### Skriva YAML

För att skriva en mapp tillbaka till en YAML-fil:

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

# Exempelanvändning
ConfigWriter.write()
# Detta kommer att skapa eller skriva över `new_config.yaml` med det angivna innehållet
```

Notera hur `yaml_elixir` möjliggör en enkel översättning mellan YAML-filer och Elixir-datastrukturer, vilket gör det till ett utmärkt val för Elixir-programmerare som behöver arbeta med YAML-data.
