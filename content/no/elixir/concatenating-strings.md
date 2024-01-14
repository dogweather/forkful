---
title:                "Elixir: Sammenføyning av strenger"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere, eller "concatenate", strenger er en vanlig oppgave i programmering, og det er viktig å forstå hvordan dette gjøres for å kunne bygge effektive og fleksible programmer. Du vil ofte måtte sette sammen deler av en streng for å skape en komplett melding eller uttrykk.

## Slik gjør du det

I Elixir-brukergrensesnittet kan du enkelt kombinere strenger ved hjelp av "+" -operatøren. La oss si at vi ønsker å lage en velkomstmelding som sier "Hei, <bruker>!" Vi kan bruke følgende kode:

```Elixir
bruker = "John"
"Hei, " + bruker + "!"
```

Dette vil gi oss følgende resultat når vi kjører koden:

```Elixir
Hei, John!
```

Vi kan også kombinere flere strenger sammen. La oss si at vi vil lage en melding som sier "Hei, <bruker>, du er velkommen til Elixir-programmering!" Vi kan bruke flere "+" -operatører på følgende måte:

```Elixir
bruker = "John"
"Hei, " + bruker + ", du er velkommen til " + "Elixir-programmering!"
```

Dette vil gi oss følgende resultat:

```Elixir
Hei, John, du er velkommen til Elixir-programmering!
```

Vi kan også bruke "<<" -operatøren for å kombinere strenger. La oss se på det samme eksemplet som ovenfor, men nå med "<<" -operatøren:

```Elixir
bruker = "John"
<<"Hei, ", bruker, ", du er velkommen til ", "Elixir-programmering!">>
```

Som du kanskje legger merke til, trenger vi ikke lenger å bruke "+" -operatøren. Resultatet vil være det samme som ovenfor.

## Dykk dypere

I tillegg til "+" og "<<" -operatørene, kan vi også bruke funksjonen `String.concat/1` for å kombinere strenger. Dette kan være nyttig hvis vi har en liste med strenger som vi ønsker å kombinere.

La oss si at vi har en liste med navn på forskjellige språk:

```Elixir
språk = ["Elixir", "Python", "Java", "JavaScript"]
```

Vi kan bruke `String.concat/1` til å lage en setning som sier "Jeg elsker å programmere i <språk>!" ved hjelp av følgende kode:

```Elixir
"Jeg elsker å programmere i " <> String.concat(språk) <> "!"
```

Resultatet vil være:

```Elixir
Jeg elsker å programmere i ElixirPythonJavaJavaScript!
```

Vi kan også legge til mellomrom mellom hvert språk ved å bruke `String.concat/2` og klamme oss rundt et mellomrom:

```Elixir
"Jeg elsker å programmere i " <> String.concat(språk, " ") <> "!"
```

Resultatet vil være:

```Elixir
Jeg elsker å programmere i Elixir Python Java JavaScript!
```

## Se også

- [Elixir dokumentasjon om strenger](https://hexdocs.pm/elixir/String.html)
- [Elixir string concatenation cheatsheet](https://devhints.io/elixir-string-concat)