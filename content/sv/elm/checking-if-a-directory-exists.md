---
title:                "Elm: Kontrollera om en mapp finns"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kolla om en mapp existerar kan vara användbart i många programmeringsscenarion. En vanlig orsak kan vara att förhindra duplicering av data eller undvika krascher.

## Hur man gör

Att kolla om en mapp existerar i Elm är en enkel process med hjälp av funktionen `Directory.exists`. Här är ett exempel på hur man använder den:

```Elm
import Directory exposing (exists)

main : Program ()
main =
    exists "mappens/namn"
        |> Task.perform handleResult

handleResult : Bool -> Cmd msg
handleResult exists =
    if exists then
        -- Gör något om mappen existerar
    else
        -- Gör något annat om mappen inte existerar
```

Ett annat sätt att använda `Directory.exists` är genom att använda den inuti en `Cmd` i en `update`-funktion och sedan hantera resultatet på liknande sätt.

## Djupdykning

Det är viktigt att notera att `Directory.exists` enbart kollar om mappen existerar, inte om det är en verklig mapp eller en symbolisk länk. Om du behöver kontrollera detta, kan du använda funktionen `Directory.isFile` eller `Directory.isSymbolicLink` beroende på vad du letar efter.

## Se även

[Hemsida för Elm](https://elm-lang.org/)\
[Dokumentation för Directory-paketet](https://package.elm-lang.org/packages/elm/core/latest/Directory#exists)