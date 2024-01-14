---
title:    "Elm: Opprette en midlertidig fil"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Det å opprette midlertidige filer kan være nyttig når man trenger å lagre data midlertidig for senere å slette dem, eller når man ønsker å lagre data som bare skal være tilgjengelig under en bestemt kjøring av programmet. Dette kan bidra til å holde filsystemet mer organisert og ryddig.

# Hvordan

For å opprette en midlertidig fil i Elm, kan vi bruke funksjonen `File.temp` fra `elm/file`-pakken. Denne funksjonen tar inn en `String` som navn på filen, og retunerer en `Cmd Msg` som kan bli sendt til Elm sin runtime for å opprette filen.

```Elm
import File exposing (temp)
import Task

createTempFile : Cmd Msg
createTempFile =
    temp "min_temp_fil.txt"
        |> Task.perform NewTempFileCreated
```

Når kommandoen `createTempFile` blir kjørt, vil det bli opprettet en midlertidig fil med navnet "min_temp_fil.txt". Vi bruker `Task.perform` for å håndtere resultatet av `temp`-funksjonen, og sender en `Msg` tilbake til vår Elm applikasjon.

# Dypdykk

Når man oppretter en midlertidig fil i Elm, vil filen bli lagret i et midlertidig område på operativsystemet. Hvor dette området er avhenger av hvilket operativsystem man bruker, og kan være forskjellig fra gang til gang. Det er derfor viktig å ikke anta at midlertidige filer vil bli lagret på et bestemt sted.

Det er også viktig å huske på at midlertidige filer ikke vil bli slettet automatisk når programmet avsluttes. Det er programmererens ansvar å slette filene når man er ferdig med å bruke dem, ved å sende en `Cmd` som utfører en sletteoperasjon.

# Se også

- [Dokumentasjon for elm/file-pakken](https://package.elm-lang.org/packages/elm/file/latest/)
- [Offisiell Elm nettside](https://elm-lang.org/)
- [Elm community Slack-kanal](https://elmlang.slack.com/)
- [Elm Norge - Facebook-gruppe](https://www.facebook.com/groups/1061660403920568/)