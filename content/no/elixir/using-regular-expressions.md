---
title:    "Elixir: Å bruke regulære uttrykk"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor

Regulære uttrykk er et nyttig verktøy for å finne og manipulere tekststrenger i Elixir-programmering. De kan hjelpe deg med å finne og endre tekst i store datamengder, og er et viktig verktøy for et effektivt og strukturert kodebibliotek.

## Hvordan

For å bruke regulære uttrykk i Elixir, må vi først importere Regex-modulen:

```Elixir
import Regex
```

Et regulært uttrykk er en sekvens av karakterer som definerer et mønster som vi ønsker å finne i en tekststreng. For eksempel, hvis vi ønsker å finne alle tall i en tekststreng, kan vi bruke følgende uttrykk:

```Elixir
numbers_regex = \d+
```

Dette vil matche alle tall i tekststrengen og returnere dem som en liste. La oss se på et eksempel:

```Elixir
text = "Jeg elsker å leke med tallene 123 og 456"
Regex.run(numbers_regex, text, capture: :all) # => ["123", "456"]
```

I dette eksempelet bruker vi også `Regex.run/3`-funksjonen for å kjøre vårt regulære uttrykk på teksten og returnere resultatet som en liste. Du kan også bruke `Regex.scan/2` for å finne alle forekomster av mønsteret og returnere dem som en liste av lister.

## Dypdykk

Regulære uttrykk kan også brukes til å gjøre avanserte søk og erstatninger. For eksempel kan vi bruke `Regex.replace/4`-funksjonen til å erstatte alle forekomster av et mønstertekst med en annen tekst. La oss si at vi ønsker å bytte ut alle tall i en tekststreng med ordet "nummer":

```Elixir
text = "Jeg elsker å leke med tallene 123 og 456"
Regex.replace(numbers_regex, text, "nummer") # => "Jeg elsker å leke med nummer og nummer"
```

Vi kan også bruke grupper i vårt regulære uttrykk for å få tilgang til og manipulere ulike deler av en tekststreng. Dette kan være nyttig for å bygge mer avanserte uttrykk. For mer informasjon og eksempler på bruk av regulære uttrykk i Elixir, kan du sjekke ut Elixir dokumentasjonen og forskjellige online ressurser.

## Se også

- [Elixir dokumentasjon om regulære uttrykk](https://hexdocs.pm/elixir/Regex.html)
- [Regular Expression Tutorial på "Regular-Expressions.info"](https://www.regular-expressions.info/elixir.html)
- [Blogginnlegg om regulære uttrykk i Elixir fra "Elixir School"](https://elixirschool.com/en/lessons/advanced/regex/)