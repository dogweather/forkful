---
title:                "Bruke regulære uttrykk"
html_title:           "Elm: Bruke regulære uttrykk"
simple_title:         "Bruke regulære uttrykk"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Regulære uttrykk er verktøy som hjelper programmerere å gjenkjenne, manipulere og matche tekststrenger. De brukes bringer effektivitet og presisjon i tekstbehandlingen.

## Hvordan:

Her er noen grunnleggende eksempler på hvordan du kan bruke regulære uttrykk i Elixir.

```elixir
# Matche et ord i en tekststreng
Regex.match?(~r/hallo/, "Hallo verden") # ==> true

# Finn og erstatt tekst
String.replace("Elixir er kult", ~r/kult/, "fantastisk") # ==> "Elixir er fantastisk"

# Splitt en tekststreng ved forhåndsdefinerte tegn
String.split("En-to-tre", ~r/-/) # ==> ["En", "to", "tre"]
```

## Dypdykk 

Regulære uttrykk kommer fra teoretisk datavitenskap og har hundreårs historie bak seg. De er implementert i nesten all programmeringspråk, inkludert Elixir. Regex-modulen i Elixir bruker PCRE (Perl Compatible Regular Expressions) biblioteket som gir tilgang til avanserte sammenkoblingsfunksjoner.

Alternativer til regulære uttrykk inkluderer strengmanipulasjoner ved hjelp av innebygde funksjoner og kraftigere parserbiblioteker. Men for de fleste praktiske behov gir regulære uttrykk et enkelt og kraftig alternativ.

Elixir's regulære uttrykk er vanligvis 'greedy'. Det betyr at de vil trenge den lengste mulige matchen. Dette er viktig å huske når du designer dine mønstre.

## Se Også 

[Elixir offisielle Regex-dokumentasjon](https://hexdocs.pm/elixir/Regex.html)

[PragDave's online kurs om Elixir](https://pragdave.podia.com/categories/elixir)

[LearnElixir's Regex-utorial](https://www.learnelixir.tv/courses/elixir-foundations/episodes/elixir-regular-expressions)