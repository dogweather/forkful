---
title:                "Loggføring"
date:                  2024-01-26T01:02:56.841680-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggføring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/logging.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Logging er i bunn og grunn prosessen med å registrere hendelser og datautskrifter fra et stykke programvare mens det kjører, tenk på det som programvarens dagbok. Programmører bruker logging for å holde oversikt over hva som skjer under hetten - det er uvurderlig for feilsøking, overvåking av systematferd i sanntid og analyse av tidligere aktivitet for ytelsesoptimaliseringer eller revisjoner.

## Hvordan gjøre det:
Elms arkitektur støtter ikke sideeffekter som logging rett ut fra esken – du håndterer dem gjennom kommandoer, som er en del av programmets arkitektur. For opplæringsformål, la oss sjekke hvordan du kan simulere logging ved å sende meldinger til JavaScript gjennom porter.

Først vil du definere en portmodul:

```Elm
port module Logger exposing (..)

-- Definer en port for å sende logger ut til JavaScript
port log : String -> Cmd msg
```

I din `Main.elm`, ville du bruke `log` porten for å sende ut en loggmelding:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- noen oppdateringer til modellen din her
            ( updatedModel, log "AnEvent occurred." )

        AnotherEvent ->
            -- andre modell oppdateringer her
            ( anotherUpdatedModel, log "AnotherEvent occurred." )
```

På JavaScript-siden, ville du abonnere på `log` porten for å håndtere de innkommende loggmeldingene:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

Eksempelutskrift i JavaScript-konsollen ville da være:

```
AnEvent occurred.
AnotherEvent occurred.
```

## Dypdykk
Tradisjonelt, i språk som Python eller Java, blir logging gjort ved å bruke et loggingbibliotek, som tilbyr et enkelt API for å logge meldinger på forskjellige nivåer som debug, info, warning, error og critical.

Elm, med sitt fokus på renhet og uforanderlighet, tilbyr ikke denne typen direkte logging, ettersom enhver type IO eller sideeffekt håndteres tydelig gjennom Elms arkitektur.

Når du trenger funksjonsrik logging i Elm, stoler du typisk på eksterne JavaScript-verktøy. Porter, som vist over, er broen til disse verktøyene. Debug-modulen er et annet alternativ, men den er ment for utviklingsbruk kun og ikke for produksjonslogging.

I tillegg til porter, bruker programmerere ofte Elms kompilatormeldinger og kjøretids feilsøkingsfasiliteter, som `Debug.log`, som du kan sette inn i koden din for å spore verdier. Den omslutter et uttrykk og logger utdataene til konsollen slik:

```Elm
view model =
    Debug.log "Model Debug" model
    -- din visningskode her
```

Dette er imidlertid heller ikke ment for produksjon. Verktøy som elm-logger gir noen abstraksjoner over porter for logging, selv om disse også er mer ment for utvikling enn produksjon.

## Se Også
- Elm porter: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Elm-diskurs om logging: https://discourse.elm-lang.org/t/elm-and-logging/546
- JavaScript Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- elm-logger-pakke: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
