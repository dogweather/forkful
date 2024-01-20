---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:56:20.679011-07:00
html_title:           "Fish Shell: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns är processen att verifiera en sökvägs existens i filsystemet. Programmerare gör detta för att försäkra sig om att filoperationer, som att läsa eller skriva filer, inte misslyckas på grund av saknade kataloger.

## Hur man gör:
Eftersom Elm är inriktad på front-end utveckling och körs i webbläsare, hanterar det inte direkt åtkomst till filsystemet. Du kan dock kommunicera med en server via HTTP för att kontrollera en katalog. Här är ett fiktivt exempel:

```Elm
import Http
import Json.Decode as Decode

type Msg = DirectoryExists Bool | DirectoryCheckFailed Http.Error

checkDirectory : String -> Cmd Msg
checkDirectory url =
    Http.get
        { url = url
        , expect = Http.expectJson (Decode.map DirectoryExists Decode.bool)
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DirectoryExists exists ->
            ( { model | directoryExists = Just exists }
            , Cmd.none
            )

        DirectoryCheckFailed _ ->
            ( { model | directoryExists = Just False }
            , Cmd.none
            )

-- Antag att servern svarar med en enkel JSON: { "exists": true } eller { "exists": false }
```

I exemplet görs ett anrop till en server Endpoint som förväntar sig en JSON med en boolean `exists`-nyckel för att signalera om katalogen finns.

## Fördjupning
I Elm's värld hanterar vi inte direkt filsystemet eftersom det körs i en webbläsarmiljö. Historiskt sett har tillgång till filsystemet begränsats i klient-side programmering på grund av säkerhetsrisker. Alternativt, kan du använda JavaScript interop-funktionen, `ports`, för att kommunicera med JavaScript-kod som har tillgång till filsystemet (i en Node.js-miljö). Implementationen kräver att du skickar meddelanden mellan Elm och JavaScript, där JS-sidan kan använda inbyggda API:er som `fs` för att kontrollera filsystemet.

## Se även
- Elm's officiella dokumentation om ports: https://guide.elm-lang.org/interop/ports.html
- MDN Web Docs om webbläsarens begränsningar: https://developer.mozilla.org/en-US/docs/Learn/Common_questions/What_is_a_web_server
- Node.js `fs` dokumentation för filsystemet: https://nodejs.org/api/fs.html