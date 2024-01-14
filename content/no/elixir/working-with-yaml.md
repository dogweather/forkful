---
title:                "Elixir: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

### Hvorfor

Å jobbe med YAML kan være en nyttig ferdighet for enhver utvikler, uansett om du utvikler webapplikasjoner, infrastruktur eller automatiseringsskript. YAML er et enkelt og leselig format for å beskrive datastrukturer, som kan brukes til å konfigurere og organisere komplekse systemer.

### Slik gjør du det

For å kunne jobbe med YAML i Elixir, trenger du_yaml_ biblioteket. Dette kan enkelt installeres ved hjelp av hex-pakker eller ved å legge det inn i ditt prosjekts "mix.exs" file. Etter installasjonen kan du begynne å bruke funksjoner fra biblioteket til å lese og skrive YAML-filer.

```Elixir
# Eksempel på hvordan du kan lese en YAML-fil og hente ut data
{:ok, data} = Yaml.decode_file("config.yaml")
IO.puts data["navn"]

# Eksempel på hvordan du kan skrive til en YAML-fil
data = %{"navn" => "Elixir", "versjon" => "1.11.2"}
Yaml.encode_file("config.yaml", data)
```

### Dykk dypere

YAML har et enkelt og intuitivt syntax som gjør det mulig å beskrive komplekse datastrukturer på en lesbar måte. Du kan bruke ulike datatyper som lister, kart og strenger, samt inkludere kommentarer og referanser mellom forskjellige deler av YAML-filen.

I tillegg til å lese og skrive til YAML-filer, kan du også bruke YAML til å konfigurere ditt Elixir-prosjekt. Ved å legge til en "config.yaml" fil i rotmappen av prosjektet ditt, kan du definere ulike innstillinger og variabler som kan brukes i koden din.

### Se også

* [Hex-pakken for Yaml](https://hex.pm/packages/yaml)
* [Offisiell YAML-spesifikasjon](https://yaml.org/spec/1.2/spec.html)
* [Elixir dokumentasjon for YAML-biblioteket](https://hexdocs.pm/yaml/api-reference.html)