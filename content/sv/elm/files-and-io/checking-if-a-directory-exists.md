---
date: 2024-01-20 14:56:20.679011-07:00
description: "Hur man g\xF6r: Eftersom Elm \xE4r inriktad p\xE5 front-end utveckling\
  \ och k\xF6rs i webbl\xE4sare, hanterar det inte direkt \xE5tkomst till filsystemet.\
  \ Du kan dock\u2026"
lastmod: '2024-03-13T22:44:37.843742-06:00'
model: unknown
summary: "Eftersom Elm \xE4r inriktad p\xE5 front-end utveckling och k\xF6rs i webbl\xE4\
  sare, hanterar det inte direkt \xE5tkomst till filsystemet."
title: Kontrollera om en katalog finns
weight: 20
---

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
