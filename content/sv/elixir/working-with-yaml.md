---
title:                "Elixir: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

YAML är ett populärt val för att strukturera och läsa data för programmerare i Elixir. Det är intuitivt och lätt att läsa, vilket gör det till ett utmärkt verktyg för att hantera data i dina Elixir-projekt. 

## Hur man gör

För att använda YAML i Elixir, behöver du först lägga till biblioteket `YamlElixir` i ditt projekt. Du kan göra det genom att lägga till följande i din mix.exs fil:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 1.0.0"}
  ]
end
```

När du har lagt till biblioteket, kan du använda funktionen `YamlElixir.load/1` för att ladda data från en YAML-fil in i ditt Elixir-program. 

```elixir
require YamlElixir

file = File.read!("config.yaml")
data = YamlElixir.load(file)
IO.inspect data
```

## Djupdykning

Det finns många användbara funktioner i `YamlElixir` biblioteket som kan hjälpa dig att läsa och hantera YAML-data i dina Elixir-program. 

För att läsa specifika värden från en YAML-fil, kan du använda funktionen `YamlElixir.get/2` och ange vägen till värdet du vill ha som en lista av nycklar. Till exempel, om du vill ha värdet för nyckeln "user" i dina data, kan du använda följande kod: 

```elixir
user = YamlElixir.get(data, ["user"])
```

Du kan även lägga till värden i din YAML-fil genom att använda funktionen `YamlElixir.put/3` och ange vägen till den nya nyckeln och värdet du vill lägga till. Till exempel, om du vill lägga till en ny användare med namnet "Alice" i din YAML-fil, kan du använda följande kod:

```elixir
updated_data = YamlElixir.put(data, ["users"], "Alice")
File.write("config.yaml", YamlElixir.dump(updated_data))
```

## Se även

- `YamlElixir` bibliotekets dokumentation: https://hexdocs.pm/yaml_elixir/
- "How to Use YAML in Your Elixir Project": https://medium.com/swedish-elixir-community/how-to-use-yaml-in-your-elixir-project-4c85e0a1be9d