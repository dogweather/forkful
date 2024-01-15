---
title:                "Generering av tilfeldige tall"
html_title:           "Elixir: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor

Enten du skal lage et spill, teste programvare eller bare vil ha en tilfeldig verdi i en situasjon, kan det være nyttig å kunne generere tilfeldige tall. Heldigvis har Elixir et innebygd bibliotek for å generere tilfeldige tall, slik at du kan gjøre dette enkelt og pålitelig.

# Slik gjør du det

For å generere tilfeldige tall, må du først importere `:rand` biblioteket ved å legge til følgende linje øverst i filen din:

```
import :rand
```

Deretter kan du bruke `rand.uniform/2` funksjonen til å generere et tilfeldig tall mellom to gitt verdier. For eksempel, kan du bruke følgende kode for å generere et tilfeldig tall mellom 1 og 100:

```
rand.uniform(1, 100)
```

Hvis du bare vil ha et tilfeldig tall mellom 0 og 1, kan du bruke `rand.uniform/1` funksjonen:

```
rand.uniform(1)
```

Du kan også generere et tilfeldig tall fra en liste ved hjelp av `rand.elem/1` funksjonen. Denne funksjonen tar inn en liste og returnerer en tilfeldig verdi fra den. For eksempel, kan du bruke følgende kode for å velge en tilfeldig farge fra en liste:

```
colors = ["rød", "blå", "grønn", "gul"]
rand.elem(colors)
```

# Dypdykk

Bak kulissene bruker Elixir en algoritme kalt Mersenne Twister for å generere tilfeldige tall. Denne algoritmen er kjent for å produsere tilfeldige tall som er både effektive og av høy kvalitet. Elixir implementerer også et "seed" system, der du kan gi en startverdi for å få de samme tilfeldige tallene hver gang du kjører koden din.

# Se også

- Offisiell dokumentasjon for `:rand` biblioteket: https://hexdocs.pm/elixir/1.10/Random.html
- En introduksjon til Elixir for nybegynnere: https://elixirlang.org/getting-started/introduction.html