---
date: 2024-01-26 03:50:16.908429-07:00
description: "Fels\xF6kning i Elm involverar att identifiera och ta bort fel fr\xE5\
  n din kod. Programmerare g\xF6r detta f\xF6r att s\xE4kerst\xE4lla att deras applikationer\
  \ fungerar\u2026"
lastmod: '2024-03-11T00:14:11.182571-06:00'
model: gpt-4-0125-preview
summary: "Fels\xF6kning i Elm involverar att identifiera och ta bort fel fr\xE5n din\
  \ kod. Programmerare g\xF6r detta f\xF6r att s\xE4kerst\xE4lla att deras applikationer\
  \ fungerar\u2026"
title: "Att anv\xE4nda en debugger"
---

{{< edit_this_page >}}

## Vad & Varför?
Felsökning i Elm involverar att identifiera och ta bort fel från din kod. Programmerare gör detta för att säkerställa att deras applikationer fungerar korrekt och för att förbättra kodkvaliteten. Elms strikta typsystem fångar många problem vid kompileringstid, men verktyg för felsökning vid körningstid är nödvändiga för att rätta till logiska fel och oväntade beteenden.

## Hur man gör:
Elm har inte en inbyggd felsökare i traditionell mening som, säg, JavaScript har med webbläsarens utvecklarverktyg. Dock har Elm-gemenskapen byggt verktyg för att fylla detta gap. Så här kan du använda `elm-debug-transformer` för att felsöka din Elm-app:

```Elm
-- Installera elm-debug-transformer (Node-paket)

1. npm install -g elm-debug-transformer

-- Använd elm-debug-transformer för att starta din app

2. elm-debug-transformer --port=8000 dittHuvudElmFilen.elm
```

När `elm-debug-transformer` körs, skapar den en WebSocket-anslutning för loggning. Du kommer att se felsökningsinformation i din webbläsares konsol där du kan inspektera programmets datastrukturer vid givna punkter i din applikation.

I Elm 0.19 och senare kan funktionerna i `Debug`-modulen såsom `Debug.log` och `Debug.todo` hjälpa dig att spåra värden och avsiktligt markera ofärdiga delar av din kod. Så här använder du Debug.log:

```Elm
importera Debug

uppdatera : Msg -> Model -> ( Model, Cmd Msg )
uppdatera msg model =
    beroende på meddelandet av
        Inkrementera ->
            ( Debug.log "Inkrementering" { model | count = model.count + 1 }, Cmd.none )

        Dekrementera ->
            ( Debug.log "Dekrementering" { model | count = model.count - 1 }, Cmd.none )
```

Du kommer att se meddelanden som "Inkrementering" eller "Dekrementering" i din webbläsares konsol tillsammans med det nya tillståndet för `modellen`.

## Fördjupning
Elms skapare, Evan Czaplicki, syftade till att skapa ett språk där vanliga buggar skulle vara omöjliga eller lätta att fånga upp. Denna filosofi är anledningen till att Elms kärna inte inkluderar traditionella felsökningsfunktioner. Elms statiska analys och typinferens bidrar massivt till att minska körningstidsfel, vilket minskar behovet av avancerad felsökning vid körningstid. Historiska alternativ inkluderade att använda den numera inaktuella `elm-reactor` som erbjöd tidsresande felsökning – ett sätt att spola tillbaka och spela upp åtgärder i din app.

Idag hjälper verktyg som `elm-debug-transformer` och användningen av Elms `Debug`-modul till att överbrygga gapet. Även om `Debug`-modulen är avsedd att användas under utveckling endast och bör tas bort före produktionsbyggen, är det ett ovärderligt verktyg för att peka ut och logga tillståndsändringar.

Tänk på att traditionella JavaScript-felsökningstekniker, såsom brytpunkter eller steg-för-steg-exekvering, inte är direkt tillämpliga i Elm på grund av dess arkitektur och Elms körtidshantering av tillståndsuppdateringar. Elm uppmuntrar dig att strukturera ditt program så att dataflödet är klart och följer strikta typer och oföränderlighet, vilket minimerar fallen där felsökning behövs.

## Se även
- Elms officiella guide om hantering av körningstidsundantag: https://guide.elm-lang.org/error_handling/
- `elm-debug-transformer` GitHub-repositorium: https://github.com/kraklin/elm-debug-transformer
- Elm-diskussionstråd som diskuterar felsökningsstrategier: https://discourse.elm-lang.org/c/show-and-tell/debugging
- Elms `Debug`-modul dokumentation: https://package.elm-lang.org/packages/elm/core/latest/Debug
