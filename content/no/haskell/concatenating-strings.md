---
title:                "Sammenslåing av strenger"
html_title:           "Haskell: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skulle noen ønske å kombinere strenger? Vel, det er en grunnleggende operasjon i nesten alle programmeringsspråk og kan være veldig nyttig når du arbeider med tekstdata. Ved å kombinere to eller flere strenger kan du bygge opp større setninger og formattere tekst på en mer effektiv måte.

# Hvordan

For å kombinere to strenger i Haskell, bruker vi operatoren `++`. La oss ta en titt på et enkelt eksempel:

```Haskell
"Hei " ++ "verden!"
```

Dette vil resultere i følgende utgang:

```Haskell
"Hei verden!"
```

Hvis du ønsker å kombinere flere strenger, kan du enkelt gjøre det ved å legge til flere `++` operasjoner:

```Haskell
"Hei " ++ "alle " ++ "sammen!"
```

Dette vil resultere i:

```Haskell
"Hei alle sammen!"
```

Vi kan også kombinere faktiske verdier med strenger. La oss si at vi har en variabel `navn` med verdien "Marie", så kan vi kombinere denne med en streng som dette:

```Haskell
"Hei " ++ navn
```

Dette vil gi oss følgende resultat:

```Haskell
"Hei Marie"
```

Du kan også kombinere flere variabler og strenger på en lignende måte. Det er imidlertid viktig å merke seg at hvis du vil kombinere en variabel og en streng, må variabelen være av samme datatype som strengen. Ellers vil du få en feilmelding.

# Dypdykk

Nå som vi har sett på hvordan du enkelt kan kombinere to eller flere strenger, la oss se på hvordan dette fungerer under overflaten. I Haskell er strengene bare lister av tegn. Derfor kan vi bruke en listefunksjon for å kombinere strenger, som for eksempel `++`.

Det er også viktig å merke seg at selv om `++` fungerer på samme måte som "concat" funksjonen i andre språk, så har den en betydelig forskjell. I Haskell blir en ny streng opprettet hver gang du bruker `++`, i stedet for å bare legge til den eksisterende strengen. Dette kan føre til ytelsesproblemer hvis du bruker denne operatoren i løkker eller store datasett.

# Se Også

- [Haskell Tutorial](https://www.haskell.org/tutorial/) - En grundig tutorial for Haskell.
- [Learn You a Haskell](http://learnyouahaskell.com/) - En interaktiv læringsressurs for Haskell.
- [The Power of String Concatenation in Functional Languages](https://medium.com/functional-landscapes/the-power-of-string-concatenate-in-functional-languages-a262597d922) - En artikkel som dykker dypere inn i bruk av strengkonkaternering i funksjonelle programmeringsspråk.