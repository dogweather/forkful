---
title:                "Arbeide med yaml"
html_title:           "Elixir: Arbeide med yaml"
simple_title:         "Arbeide med yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med YAML kan være nyttig for å lagre og behandle datastrukturer på en enkel og lesbbar måte. Det er også en populær format for konfigurasjonsfiler i mange programmeringsspråk.

## Hvordan

For å arbeide med YAML i Elixir, må du først legge til `:yaml_erl` biblioteket i prosjektet ditt. Deretter kan du bruke funksjonen `YAML.decode/1` for å dekode en YAML-streng til en Elixir-struct.

```Elixir
require YAML
yaml = "---\nname: John\nage: 30"
YAML.decode(yaml) # %{name: "John", age: 30}
```

Å konvertere en Elixir-struct til YAML er like enkelt med `YAML.encode/1`:

```Elixir
user = %{name: "John", age: 30}
YAML.encode(user) # "---\nname: John\nage: 30"
```

Hvis du trenger å manipulere YAML-data, kan du bruke funksjonene i `YAML.Types`-modulen, for eksempel for å hente en verdi fra en nøkkel:

```Elixir
yaml = "---\nname: John\nage: 30"
YAML.get(yaml, "name") # "John"
```

## Dypdykk

Et interessant aspekt ved å jobbe med YAML i Elixir er at det gir deg muligheten til å bruke såkalte "custom types". Dette betyr at du kan mappe nestede YAML-strukturer til dine egne Elixir-structs, som kan gjøre det lettere å håndtere komplekse data.

For å opprette et slikt custom type, må du implementere protokollen `YAML.CustomType`. La oss ta et enkelt eksempel med et user-objekt som kan ha flere roller:

```Elixir
defmodule User do
  defstruct [:name, :age, roles: []]

  defimpl YAML.CustomType do
    def decode(%{"name" => name, "age" => age, "roles" => roles}) do
      %User{name: name, age: age, roles: roles}
    end

    def encode(%User{name: name, age: age, roles: roles}) do
      %{"name" => name, "age" => age, "roles" => roles}
    end
  end
end
```

Nå kan vi bruke `YAML.decode/1` til å konvertere en YAML-streng til en `User`-struct:

```Elixir
yaml = "---\nname: John\nage: 30\nroles:\n  - admin\n  - editor"
YAML.decode(yaml) # %User{name: "John", age: 30, roles: ["admin", "editor"]}
```

## Se også

- [YAML-dokumentasjon](https://yaml.org/)
- [Elixir-YAML dokumentasjon](https://hexdocs.pm/yaml/readme.html) 
- [Elixir dokumentasjon](https://hexdocs.pm/elixir/api-reference.html)