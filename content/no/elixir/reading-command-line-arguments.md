---
title:    "Elixir: Lesing av kommandolinjeargumenter"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor
Bloggposten vår i dag vil handle om å lese kommandolinjeargumenter i Elixir. Å forstå og kunne håndtere kommandolinjeargumenter er avgjørende for å skrive fleksible og nyttige programmer, så dette er et viktig konsept å lære for alle som ønsker å utvikle seg som Elixir-programmere.

## Slik gjør du det
For å starte, la oss se på et enkelt eksempel på å lese et enkelt kommandolinjeargument og skrive ut det til konsollen:

```Elixir
args = System.argv()
IO.puts("Det første argumentet er: #{args[0]}")
```

I dette tilfellet bruker vi `System.argv/0`-funksjonen for å hente alle kommandolinjeargumentene som en liste. Vi kan deretter bruke indeksering for å få tilgang til spesifikke argumenter, og i dette tilfellet skrive ut det første argumentet ved hjelp av `IO.puts/1`-funksjonen.

La oss nå se på et mer komplekst eksempel som tar imot flere argumenter og håndterer dem med en `case`-konstruksjon:

```Elixir
args = System.argv()
case args[0] do
  "hello" ->
    IO.puts("Hei!")
  "goodbye" ->
    IO.puts("Ha det!")
  _ ->
    IO.puts("Jeg forstår ikke hva du mener.")
end
```

Her bruker vi `case`-konstruksjonen for å håndtere ulike kommandolinjeargumenter og skrive ut forskjellige meldinger basert på hva argumentet er. Den siste `case`-grenen med ` _ ` som matcher all hvilken verdien returnerer en generisk melding.

## Dykk dypere
Nå som vi har sett på noen enkle eksempler, la oss se på noen flere detaljer om kommandolinjeargumenter i Elixir. Det første er at `System.argv/0`-funksjonen returnerer en liste med argumenter, og ikke inkluderer navnet på selve programmet. Dette betyr at første element i listen vil være det første faktiske argumentet som brukes i programmet ditt.

En annen nyttig funksjon er `System.get_env/2` som lar deg få tilgang til miljøvariabler fra kommandolinjen. For eksempel, hvis du vil sjekke om et argument ble passert inn til programmet ditt, kan du gjøre noe som dette:

```Elixir
case System.get_env("MY_FLAG") do
  "true" ->
    IO.puts("My flag is set!")
  _ ->
    IO.puts("My flag is not set.")
end
```

Med denne koden, hvis du kjører programmet ditt med et kommandolinjeargument `MY_FLAG=true`, vil du få utskriften "My flag is set!", ellers vil du få "My flag is not set." Dette kan være veldig nyttig for testing og konfigurering av programmer.

## Se også
- [Command Line Interfaces in Elixir](https://elixir-lang.org/getting-started/command-line.html)
- [Elixir System module documentation](https://hexdocs.pm/elixir/System.html)
- [Tutorial: Command Line Arguments in Elixir](https://yurisubach.com/articles/command-line-arguments-elixir-examples/)