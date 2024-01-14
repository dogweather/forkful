---
title:    "Elixir: Sammenstilling av strenger."
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ville slå sammen strenger? Vel, det er et nyttig verktøy når du ønsker å kombinere ulike tekster eller variabler til en enkelt streng. Dette kan være nyttig når du skal generere dynamiske tekster for brukere eller sette sammen deler av en URL for å kommunisere med en API.

## Hvordan

I Elixir bruker vi "+" operatøren for å slå sammen strenger. La oss se på et eksempel:

```Elixir
string1 = "Hei"
string2 = "verden!"

resultat = string1 + " " + string2

IO.puts(resultat)
```

Dette vil gi følgende output:

```
Hei verden!
```

Vi kan også bruke `<>` operatøren for å få samme resultat:

```Elixir
string1 = "Hei"
string2 = "verden!"

resultat = string1 <> " " <> string2

IO.puts(resultat)
```

Ved å bruke variabler sammen med tekst, kan vi dynamisk generere tekster som passer med ulike verdier. La oss bruke `String.length` for å telle antall tegn i en variabel og inkludere det i vår streng:

```Elixir
string = "Hei verden!"

antall_tegn = String.length(string)

resultat = "Teksten inneholder " <> antall_tegn <> " tegn."

IO.puts(resultat)
```

Dette vil gi oss følgende output:

```
Teksten inneholder 11 tegn.
```

## Dypdykk

Når vi slår sammen strenger i Elixir, skjer det faktisk en konvertering til en ny datastruktur kalt `Binary`. Dette er for å gjøre slå sammen av strenger raskere og mer effektivt. Det betyr også at vi kan slå sammen andre datastrukturer som lister eller tuples til en streng ved hjelp av `to_string()` funksjonen.

La oss se hvordan vi kan slå sammen en liste av tall til en enkelt streng:

```Elixir
liste = [1, 2, 3]

resultat = "Tallene er: " <> to_string(liste)

IO.puts(resultat)
```

Dette vil gi følgende output:

```
Tallene er: [1, 2, 3]
```

Vi kan også bruke `Enum.join()` funksjonen for å kombinere elementene i en liste og separere dem med et spesifisert tegn. La oss bruke komma som seperator:

```Elixir
liste = [1, 2, 3]

resultat = "Tallene er: " <> Enum.join(liste, ",")

IO.puts(resultat)
```

Dette vil gi følgende output:

```
Tallene er: 1,2,3
```

## Se også

Nå når vi har lært å slå sammen strenger i Elixir, kan det være nyttig å utforske andre funksjoner og konsepter i språket. Her er noen ressurser som kan være nyttige:

- [Elixir Official Documentation](https://hexdocs.pm/elixir/)
- [Learn Elixir in Y minutes](https://learnxinyminutes.com/docs/elixir/)
- [Elixir School](https://elixirschool.com/)

Takk for at du leste! Lykke til med å bruke strenger i din Elixir-kode.