---
title:    "Elm: Lese en tekstfil"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

#Hvorfor

Hvis du er en nybegynner innenfor programmering eller en erfaren utvikler på jakt etter et nytt språk å lære, bør du ta en titt på Elm. Dette funksjonelle programmeringsspråket er enkelt å lære, og det har mange nyttige funksjoner. I denne bloggposten vil vi fokusere på hvordan du kan lese en tekstfil ved hjelp av Elm.

#Slik gjør du det

For å lese en tekstfil i Elm, må du følge disse trinnene:

1. Importer File modulen ved å legge til dette øverst i filen din: ```import File exposing (readString)```

2. Bruk funksjonen ```readString``` for å lese innholdet av tekstfilen. Denne funksjonen tar to argumenter: filbanen og en funksjon som beskriver hva som skal gjøres med tekststrengen (for eksempel å skrive den ut til konsollen). Et eksempel på hvordan du kan bruke denne funksjonen: 

```
readString "minfil.txt" (\result -> 
    case result of 
        Ok content -> 
            Debug.log "Tekstfilinnholdet er" content 
        Err error -> 
            Debug.log "Det oppsto en feil: " error 
)
```

3. Nå kan du kjøre koden din for å se resultatet!

Eksempel på tekstfilinnhold (minfil.txt):

```
Hei! Dette er en tekstfil.
Den inneholder litt informasjon som vi skal lese med Elm.
```

Output i konsollen:

```
Tekstfilinnholdet er Hei! Dette er en tekstfil. Den inneholder litt informasjon som vi skal lese med Elm.
```

#Dykke dypere

Nå som du vet hvordan du kan lese en enkel tekstfil i Elm, kan det være nyttig å vite om noen av de andre funksjonene og metodene for å lese filer. For eksempel kan du bruke ```readLines``` for å lese en tekstfil linje for linje, eller ```readAsStream```, som lar deg lese store filer stykkevis og kontinuerlig. 

Du kan også bruke funksjonen ```readFile``` for å lese binærfiler, eller ```readDirectory``` for å lese innholdet av en hel mappe. Det er også mulig å lese filer fra internett ved å bruke funksjoner som ```Http.send```.

Det er verdt å merke seg at disse funksjonene returnerer resultatet i form av en ```Task```, som må håndteres riktig for å unngå eventuelle feil.

#Se også

- Elm's offisielle dokumentasjon for File modulen: https://package.elm-lang.org/packages/elm/file/latest/
- Enkel Elm guide for å lese tekstfiler: https://guide.elm-lang.org/effects/files.html
- Elm tutorial for å lese og skrive filer: https://www.tutorialspoint.com/elm/elm_file_handling.htm