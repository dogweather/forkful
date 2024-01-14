---
title:                "Elixir: Å starte et nytt prosjekt"
programming_language: "Elixir"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor starte et nytt prosjekt med Elixir?

Elixir er et spennende programmeringsspråk som brukes til å lage skalerbare, feiltolerante og distribuerte systemer. Det er inspirert av språket Erlang og bygger på funksjonell programmering. Å starte et prosjekt med Elixir gir mulighet for å bygge et holdbart og pålitelig system som kan håndtere store mengder data og brukere.

## Slik gjør du det

For å starte et nytt prosjekt med Elixir, må du først installere språket og Elixir-miljøet på datamaskinen din. Deretter kan du følge disse enkle stegene:

1. Lag en ny mappe for prosjektet ditt og naviger til denne mappen i terminalen.
2. Kjør kommandoen `mix new project_name` for å lage et nytt Elixir-prosjekt.
3. Nå er det på tide å sette i gang med kode! Åpne filen `lib/project_name.ex` og begynn å eksperimentere med din første funksjon.

Her er et eksempel på hvordan en enkel funksjon kan se ut i Elixir:

```Elixir
def hello(name) do
  "Hei, #{name}!"
end
```
I denne funksjonen tar vi inn et navn som parameter, og returnerer en streng med en hilsen. For å teste dette, kan du kjøre kommandoen `iex -S mix` i terminalen for å åpne Elixir sin interaktive skall, og deretter kjøre `Hello.hello("Navn")` for å se output.

## Dykk dypere

Etter å ha fått en smakebit på Elixir sin syntaks og funksjonaliteter, er det på tide å dykke dypere inn i språket og utforske dets mange unike funksjoner. Elixir har for eksempel et robust mønstermatching-system og et kraftig asynkront jobbsystem ved hjelp av "beverages". Å utforske disse og andre funksjoner vil hjelpe deg å bygge mer effektive og skalerbare applikasjoner.

En annen fordel med å bruke Elixir er at det kjører på Erlang virtuell maskin (BEAM) som er kjent for å være svært stabilt og feiltolerant. Dette betyr at applikasjonene dine vil kunne håndtere feil og kjøre jevnt uten å krasje.

## Se også

Du kan lære mer om Elixir og begynne å bygge dine egne prosjekter ved å sjekke ut disse ressursene:

- [Offisiell Elixir-nettside](https://elixir-lang.org/)
- [Elixir School](https://elixirschool.com/no/)
- [Elixir forum](https://elixirforum.com/)
- [Elixircast podcast](https://elixircast.com/)

Lykke til med ditt Elixir-prosjekt! 😉