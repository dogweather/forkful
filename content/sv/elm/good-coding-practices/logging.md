---
date: 2024-01-26 01:02:39.024845-07:00
description: "Loggning \xE4r i grund och botten processen att spela in h\xE4ndelser\
  \ och datautg\xE5ngar fr\xE5n en mjukvara n\xE4r den k\xF6rs, t\xE4nk p\xE5 det\
  \ som mjukvarans dagbok.\u2026"
lastmod: '2024-03-13T22:44:37.835988-06:00'
model: gpt-4-1106-preview
summary: "Loggning \xE4r i grund och botten processen att spela in h\xE4ndelser och\
  \ datautg\xE5ngar fr\xE5n en mjukvara n\xE4r den k\xF6rs, t\xE4nk p\xE5 det som\
  \ mjukvarans dagbok.\u2026"
title: Loggning
---

{{< edit_this_page >}}

## Vad & Varför?
Loggning är i grund och botten processen att spela in händelser och datautgångar från en mjukvara när den körs, tänk på det som mjukvarans dagbok. Programmerare använder loggning för att hålla koll på vad som händer under huven - det är ovärderligt för att felsöka problem, övervaka systembeteende i realtid och analysera tidigare aktivitet för prestandaoptimeringar eller revisioner.

## Hur man gör:
Elms arkitektur stöder inte sidoeffekter som loggning direkt ur lådan - du hanterar dem genom kommandon, som är en del av din applikations arkitektur. I utbildningssyfte, låt oss kontrollera hur du kan simulera loggning genom att skicka meddelanden till JavaScript genom portar.

Först definierar du en portmodul:

```Elm
port module Logger exposing (..)

-- Definiera en port för att skicka loggar till JavaScript
port log : String -> Cmd msg
```

I din `Main.elm` skulle du använda `log`-porten för att skicka ut ett loggmeddelande:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- vissa uppdateringar till din modell här
            ( updatedModel, log "AnEvent inträffade." )

        AnotherEvent ->
            -- andra modelluppdateringar här
            ( anotherUpdatedModel, log "AnotherEvent inträffade." )
```

På JavaScript-sidan skulle du prenumerera på `log`-porten för att hantera de inkommande loggmeddelandena:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

Exempelutmatning i JavaScript-konsolen skulle då bli:

```
AnEvent inträffade.
AnotherEvent inträffade.
```

## Fördjupning
Traditionellt sett, i språk som Python eller Java, görs loggning genom att använda ett loggningsbibliotek, vilket tillhandahåller ett enkelt API för att logga meddelanden på olika nivåer såsom debug, info, varning, fel och kritisk.

Elm, med sitt fokus på renhet och oföränderlighet, tillhandahåller inte denna typ av direkt loggning, eftersom alla slag av I/O eller sidoeffekter hanteras distinkt genom Elm-arkitekturen.

När du behöver fullfjädrad loggning i Elm, lutar du dig typiskt mot externa JavaScript-verktyg. Portar, som visats ovan, är bron till dessa verktyg. Debug-modulen är ett annat alternativ, men den är avsedd enbart för utvecklingsbruk och inte för produktionsloggning.

Förutom portar använder programmerare ofta Elms kompilerarmeddelanden och körtidsfelsökningsfunktioner, som `Debug.log`, som du kan infoga i din kod för att spåra värden. Den omsluter ett uttryck och loggar dess utvärde till konsolen så här:

```Elm
view model =
    Debug.log "Model Debug" model
    -- din kod för vy här
```

Detta är dock inte heller menat för produktion. Verktyg som elm-logger tillhandahåller vissa abstraktioner över portar för loggning, även om dessa också är avsedda mer för utveckling än för produktion.

## Se även
- Elm-portar: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Elm-diskussion om loggning: https://discourse.elm-lang.org/t/elm-and-logging/546
- JavaScript Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- elm-logger-paket: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
