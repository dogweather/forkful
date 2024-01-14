---
title:                "Haskell: Sammenslåing av strenger"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Som programmerere har vi ofte behov for å kombinere tekststrenger for å skape nyttige utskrifter og meldinger. Dette prosessen, kjent som å "konkatenere" strenger, er en viktig del av å bygge funksjonelle programmer. I Haskell, er dette en enkel oppgave takket være funksjoner som `++` og `concat`.

## Hvordan

Før vi dykker inn i dybden av å konkatenere strenger i Haskell, la oss først se på et enkelt eksempel for å forstå konseptet:

```Haskell
concat "Hei" "verden"
```

Dette vil resultere i utskriften `Hei verden`. Her er begge strengene kombinert ved hjelp av funksjonen `concat`, som tar to strenger som input og returnerer en kombinert streng som output. 

For å konkatenerere mer enn to strenger, kan vi bruke `++` funksjonen som tar to lister av tegn som input og kombinerer dem til én. For eksempel:

```Haskell
"Hallo" ++ " " ++ "Norge"
```

Dette vil skrive ut `Hallo Norge`, da `++` kombinerer hver streng med et mellomrom imellom. Du kan også bruke `++` til å konkatenere en streng med en liste av tegn, som vist i dette eksempelet:

```Haskell
"Hallo" ++ [' ', 'V', 'e', 'r', 'd', 'e', 'n']
```

Dette vil gi samme resultat som forrige eksempel, altså `Hallo Verden`.

## Dykk dypere

Når vi bruker funksjonene `++` og `concat`, må vi være oppmerksomme på at de operer på lister av tegn og ikke bare på strenger. Dette betyr at for å konkatenere strenger, må vi konvertere dem til lister først. Dette kan gjøres ved hjelp av funksjonen `words`, som tar en streng som input og returnerer en liste av ord. For eksempel:

```Haskell
concat (words "Hei, verden!")
```

Dette vil skrive ut `Hei, verden!` siden `words` konverterer strengen til en liste av ord, og deretter konkatenere dem til én streng ved hjelp av `concat`.

En annen viktig ting å være klar over er at begge funksjonene `++` og `concat` kun kan konkatenere lister av tegn og ikke andre datatyper som tall eller boolske verdier. For å konkatenere disse datatypene, må vi først konvertere dem til en streng og deretter bruke funksjonene som beskrevet over.

## Se også

- [Offisiell Haskell dokumentasjon](https://www.haskell.org/documentation/)
- [Haskell tutorials på norsk](http://haskell.no/tutorials/)
- [Konkatenere strenger i Haskell - Stack Overflow spørsmål](https://stackoverflow.com/questions/20496811/konkatenere-strenger-i-haskell)