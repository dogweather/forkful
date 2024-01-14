---
title:    "Elixir: Generering av tilfeldige tall"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor
Å generere tilfeldige tall er en viktig del av mange programmeringsspråk, og Elixir er intet unntak. Tilfeldige tall kan brukes til å lage unike ID-nummer, tilfeldig valg av elementer fra lister, og mye mer. Det er en nyttig verktøy i mange ulike situasjoner og kan bidra til å gjøre programmer mer varierte og interessante.

# Hvordan
For å generere tilfeldige tall i Elixir, kan du bruke funksjonene i Elixir's `:rand` modul. Her er et enkelt eksempel som genererer et tilfeldig tall mellom 1 og 10:

```Elixir
  :rand.uniform(1..10)
```

Dette vil returnere et tilfeldig heltall mellom 1 og 10, og det er så enkelt som det!

Du kan også angi et seed-nummer for å få tilfeldige tall i en bestemt rekkefølge. Dette er nyttig hvis du for eksempel ønsker å teste forskjellige scenarier med det samme settet av tilfeldige tall. Her er et eksempel som angir et seed-nummer på 123 og genererer et tilfeldig tall mellom 1 og 100:

```Elixir
  :rand.seed(123)
  :rand.uniform(1..100)
```

Det er også en rekke andre funksjoner i `:rand` modulen som kan brukes til å generere tilfeldige tall i forskjellige former, som f.eks. desimaltall og strenger.

# Dypdykk
Elixir's `:rand` modul bruker en algoritme kalt "Mersenne Twister" for å generere tilfeldige tall. Denne algoritmen er i stand til å generere en stor mengde forskjellige tallsekvenser, og har en periode på over 2³¹⁸. Dette gjør det veldig usannsynlig at den vil generere samme tallsekvens igjen, og sikrer dermed en høy grad av tilfeldighet.

Det er også verdt å nevne at tilfeldigheten i tallene som genereres av `:rand` modulen ikke er helt perfekt (det er ingen virkelig tilfeldighet i en datamaskin). Men for de fleste formål i programmering, er dette nivået av tilfeldighet mer enn tilstrekkelig.

# Se Også
- Elixir's Offisielle Dokumentasjon om `:rand` Modulen: https://hexdocs.pm/elixir/Random.html 
- En Artikkel om Tilfeldige Tall i Elixir på Medium: https://medium.com/@maelvls/random-numbers-and-generators-in-elixir-2a69b4ecde1c 
- En Undervisningsvideo om Tilfeldige Tall i Elixir på YouTube: https://www.youtube.com/watch?v=ehyPkE6GzEk