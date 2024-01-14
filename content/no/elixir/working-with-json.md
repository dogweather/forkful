---
title:                "Elixir: Arbeid med json"
simple_title:         "Arbeid med json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med JSON kan åpne opp for uendelige muligheter innen Elixir-programmering. JSON er en populær måte å strukturere og lagre data på, og bruk av det i Elixir gir en enkel og effektiv måte å kommunisere med nettjenester.

## Hvordan

For å jobbe med JSON i Elixir, kan man først importere biblioteket `Jason` ved å legge til `{:jason, "~> 1.0"}` i `mix.exs`-filen. Dette gir oss tilgang til funksjoner som lar oss konvertere data til og fra JSON-formatet.

La oss si at vi har en liste med navn som vi ønsker å konvertere til JSON-format:

```Elixir
names = ["Ingrid", "Bjørn", "Marianne"]
```

Vi kan da bruke funksjonen `Jason.encode!/1` for å konvertere listen til JSON:

```Elixir
json = Jason.encode!(names)
```

Output av `json` variabelen vil være:

```Elixir
"[\"Ingrid\", \"Bjørn\", \"Marianne\"]"
```

For å konvertere JSON tilbake til en liste, kan vi bruke funksjonen `Jason.decode!/1`:

```Elixir
Jason.decode!(json)
```

Dette vil gi oss tilbake den opprinnelige listen med navn.

## Dypdykk

Det er også mulig å jobbe med mer komplekse strukturer i JSON-formatet. Funksjonen `Jason.decode!/1` kan for eksempel håndtere nestede objekter og lister. I tillegg kan vi bruke funksjoner som `Jason.encode!/2` for å gi mer kontroll over hvordan dataen blir konvertert til JSON.

Det er også verdt å nevne at Elixir har støtte for asynkron kommunikasjon med JSON ved hjelp av `Jason.Stream`. Dette kan være nyttig når man skal håndtere store mengder data eller ved bruk av nettverkstjenester.

## Se også

- [Jason dokumentasjon](https://hexdocs.pm/jason/1.1.2/readme.html)
- [Elixir dokumentasjon om JSON](https://elixir-lang.org/getting-started/erlang-libraries.html#json)