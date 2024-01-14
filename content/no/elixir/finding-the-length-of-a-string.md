---
title:    "Elixir: Å finne lengden av en streng"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden til en streng kan være nyttig i mange situasjoner, enten du trenger å begrense tekstlengden i en melding, konvertere data til binærform eller bare lage en funksjon som teller tegn i et ord. Uansett årsak, er det viktig å ha et solid grep om hvordan man kan finne lengden til en streng i Elixir.

## Hvordan gjøre det

For å finne lengden til en streng i Elixir, kan man bruke den innebygde funksjonen `String.length()`. Denne funksjonen tar inn en streng som argument og returnerer antallet tegn i den.

```Elixir 
# Eksempel på å finne lengden til en streng

streng = "Hei, verden!"
String.length(streng) #=> 12
```

Som du kan se, returnerer `String.length()` et heltall som representerer antallet tegn i strengen.

## Dypere dykk

Det kan også være interessant å utforske hvordan denne funksjonen fungerer bak kulissene. I Elixir er strenger i seg selv en liste av tegn, og `String.length()` fungerer ved å telle antallet elementer i denne listen. Dette kan være nyttig å vite når man jobber med større og mer komplekse strenger, da det kan påvirke ytelsen til koden din.

En annen måte å finne lengden til en streng på er å bruke `String.graphemes()`, som returnerer en liste av grafemer i strengen. Et grafem er en visuell enhet av tekst, for eksempel bokstaver og symboler. Ved å telle antallet elemeneter i denne listen, kan man finne lengden til strengen.

```Elixir 
# Eksempel på å finne lengden til en streng ved hjelp av grafemer

streng = "Hei, 🌎!"
String.graphemes(streng) #=> [
  "H", "e", "i", ",", " ", "🌎", "!"
]
```

## Se også

- [Elixir Docs: String.length()](https://hexdocs.pm/elixir/String.html#length/1)
- [Elixir Docs: String.graphemes()](https://hexdocs.pm/elixir/String.html#graphemes/1)