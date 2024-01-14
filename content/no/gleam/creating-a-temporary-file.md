---
title:                "Gleam: Oppretting av en midlertidig fil"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

#Hvorfor
Hvorfor opprette midlertidige filer i Gleam? Det kan være mange grunner til å gjøre det, enten det er for å lagre midlertidige data mens du kjører et program, eller for å dele data mellom forskjellige deler av koden din.

#Slik gjør du det
Det er enkelt å opprette midlertidige filer i Gleam! Bare bruk "File.Temp" -funksjonen og gi filen et navn og en filbane. Her er et eksempel på hvordan du kan opprette en midlertidig fil og skrive data til den:

```Gleam
let filbane = "/tmp/minmidlertidigfil"
let fil = File.Temp("minfil.txt", filbane)

File.write(fil, "Dette er dataen min")

println(File.read_to_string(fil))
```

Dette vil opprette en ny midlertidig fil med navnet "minfil.txt" i mappen "/tmp" og skrive dataen "Dette er dataen min" til den. Du kan endre filnavn og filbanen etter dine behov.

#Dykk dypere
Det er verdt å merke seg at midlertidige filer vil bli automatisk slettet når programmet ditt avsluttes. Hvis du vil slette filen manuelt, kan du bruke "File.delete" -funksjonen. Du kan også bruke "File.exist" for å sjekke om en fil eksisterer eller ikke.

En annen ting å huske på er at midlertidige filer kan være nyttige når du jobber med store mengder data, da det kan være mer effektivt å skrive til en midlertidig fil før du leser dataen tilbake. Dette bidrar til å unngå minnelekkasjer og optimaliserer ytelsen til koden din.

#Se også
- [Gleam dokumentasjon](https://gleam.run/docs/)
- [Hvordan håndtere filer effektivt i Gleam](https://komprogrammerar.wordpress.com/2020/05/14/how-to-handle-files-efficiently-in-gleam/)