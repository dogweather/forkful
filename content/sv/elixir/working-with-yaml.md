---
title:                "Att arbeta med yaml"
html_title:           "Elixir: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför
YAML är ett lättläst dataformat som är vanligtvis används för konfigurationsfiler eller för att dela data mellan applikationer. Det är ett bra val för Elixir programmerare som vill hålla sina datastrukturer enkla och lättlästa.

## Hur man använder YAML i Elixir
För att använda YAML i Elixir behöver du först installera Paketet Yaml (Yaml Package). Detta kan göras genom att lägga till följande rad i din mix.exs fil under `deps` avsnittet:
<br/>`{:yaml, "~> 0.2"}` 

### Enkelt exempel
När paketet har installerats kan du enkelt läsa in en YAML fil och få ut strukturerna i en map genom att använda `YAML.load_file/1` funktionen. Låt oss säga att vi har en `config.yaml` fil med följande innehåll:
<br/>
```
user:
    name: John
    age: 32
```

I din Elixir kod kan du läsa in filen och få ut strukturen såhär:
<br/>
```Elixir
config = YAML.load_file("config.yaml")
IO.puts config["user"]["name"]
# Output: John
```

### Avancerat exempel
Du kan också använda YAML för att skriva datastrukturer till en fil. Låt oss säga att vi vill spara en map med information om användare i en YAML fil. Vi kan göra det genom att använda `YAML.dump/2` funktionen och skicka in vår map och filnamn som argument.
<br/>
```Elixir
user = %{name: "John", age: 32}
YAML.dump(user, "user.yaml")
# Output: user.yaml filen kommer att innehålla följande innehåll:
# ---
# :name: John
# :age: 32
```

## Djupdykning i YAML
YAML stöder flera avancerade funktioner som gör det mer kraftfullt än bara att läsa och skriva datastrukturer. I YAML kan du till exempel använda referenser för att återanvända delar av din datastruktur, vilket kan vara användbart om du har mycket liknande data. Du kan också använda YAML för att definiera användardefinierade datatyper och till och med inkludera små programsnuttar som kan köras vid läsning av en fil.

## Se även
- [Yaml Package Dokumentation](https://hexdocs.pm/yaml)
- [Officiell YAML Specifikation](https://yaml.org/spec/)