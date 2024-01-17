---
title:                "Jobbe med yaml"
html_title:           "Elixir: Jobbe med yaml"
simple_title:         "Jobbe med yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
 YAML er et format for serialisering av data, som brukes av mange programmerere for å lagre og transportere datastrukturer i en lesbar form. Den er spesielt nyttig for webapplikasjoner og konfigurasjonsfiler.

## Hvordan:
Hvis du vil jobbe med YAML i Elixir, kan du bruke biblioteket YamlElixir. Først må du installere biblioteket ved å legge til `{:yaml_elixir, "~> 4.0"}` i `mix.exs` filen din. Deretter kan du konvertere data fra YAML til Elixir ved å bruke `YamlElixir.load/1` funksjonen. For eksempel:

```Elixir
yaml = "- name: Elixir
  version: 1.12"
  
ElixirYaml.load(yaml)
# => %{name: "Elixir", version: 1.12}
```
Du kan også konvertere Elixir-data til YAML ved å bruke `YamlElixir.dump/1` funksjonen. For eksempel:

```Elixir
data = %{name: "Elixir", version: 1.12}

YamlElixir.dump(data)
# => "- name: Elixir\n  version: 1.12\n"
```

## Dypdykk:
YAML ble utviklet av Ingy döt Net som et alternativ til JSON og XML for å skape et mer leselig format for mennesker. Det er også inspirert av programmeringsspråket Perl. Selv om YAML er populært i mange programmeringsspråk, har det også blitt utsatt for noen sikkerhetsproblemer, så det er viktig å være forsiktig når du bruker det i nettapplikasjoner.

Alternativt kan du også bruke Elixirs innebygde funksjoner, `:yaml.encode/1` og `:yaml.decode/1`, for å jobbe med YAML-data. Disse funksjonene bruker biblioteket libyaml som standard, som er raskere og mer sikker enn den forrige implementeringen.

## Se også:
- YamlElixir dokumentasjon: https://hexdocs.pm/yaml_elixir/4.0.0/readme.html
- Elixir YAML-API: https://hexdocs.pm/elixir/YAML.html
- Libyaml dokumentasjon: https://pyyaml.org/wiki/LibYAML