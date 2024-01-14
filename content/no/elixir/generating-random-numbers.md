---
title:                "Elixir: Generering av tilfeldige tall"
programming_language: "Elixir"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

##Hvorfor
I denne bloggposten vil vi utforske hvordan man kan generere tilfeldige tall i Elixir. Å kunne generere tilfeldige tall kan være nyttig i mange ulike programmeringssammenhenger, som for eksempel spill, simuleringer eller ved å teste ulike funksjoner.

##Hvordan
Vi kan generere tilfeldige tall i Elixir ved hjelp av funksjonen ```Enum.random/1```. Denne funksjonen tar inn en liste eller et range og returnerer et tilfeldig element fra denne listen eller rangen.

For eksempel, for å generere et tilfeldig tall mellom 1 og 10, kan vi bruke følgende kode:

```
Elixir
Enum.random(1..10)
```

Dette vil returnere et tilfeldig tall mellom 1 og 10 hver gang koden kjøres.

Vi kan også generere tilfeldige flåttall ved å bruke funksjonen ```:rand.uniform/1```. Denne funksjonen tar inn et tall og returnerer et tilfeldig tall mellom 0 og tallet.

```
Elixir
:rand.uniform(100)
```

Dette vil generere et tilfeldig tall mellom 0 og 100 hver gang koden kjøres.

##Dypdykk
Bak kulissene genererer Elixir tilfeldige tall ved hjelp av en uniform fordeling. Dette betyr at hvert tall har samme sannsynlighet for å bli returnert. Elixir bruker også en generator kalt Mersenne Twister for å generere disse tilfeldige tallene.

Det er også mulig å sette en "seed" når man genererer tilfeldige tall i Elixir. Dette betyr at man kan få de samme tilfeldige tallene hver gang koden kjøres, noe som kan være nyttig for testing.

##Se også
- [Dokumentasjon for Elixir's tilfeldige tall funksjoner](https://hexdocs.pm/elixir/Enum.html#random/1)
- [Artikkel om tilfeldige tall i Elixir av Jose Valim](https://www.josevalim.com/elixir-random-numbers/)
- [So treff du brukar Mersenne Twister gjennom Elixir](https://dailyelixir.com/post/so-brukar-du-mersenne-twister-gjennom-elixir/)