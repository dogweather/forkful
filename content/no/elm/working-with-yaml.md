---
title:                "Arbeid med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML er et dataformat for å skrive konfigurasjoner lett leslig for mennesker. Programmerere bruker det for å håndtere konfigurasjoner og datautveksling.

## How to:
Elm har ingen innebygd støtte for YAML. Men du kan bruke JavaScript-biblioteker med `ports` for parsing. Her er et eksempel som bruker `js-yaml`:

```Elm
port module Main exposing (..)

-- Definerer en port for å sende YAML til JS
port toYaml : String -> Cmd msg

-- Definerer en port for å få den konverterte JS objektet tilbake som en streng
port fromYaml : (String -> msg) -> Sub msg

-- Funksjon for å sende en YAML streng til JS
convertYaml : String -> Cmd msg
convertYaml yamlString =
    toYaml yamlString

-- Abonnerer på svaret fra JS
subscriptions : Model -> Sub Msg
subscriptions model =
    fromYaml ConvertedYaml

-- Mottar den konverterte YAML strengen som JSON
type Msg = ConvertedYaml String
```

Og på JavaScript-siden:

```javascript
// Fungerer med `elm/browser` og `ports`
app.ports.toYaml.subscribe(function(yamlString) {
    try {
        var result = jsyaml.load(yamlString);
        app.ports.fromYaml.send(JSON.stringify(result));
    } catch (e) {
        // Handle parsing errors
    }
});
```

## Deep Dive
YAML kom i 2001 og tilbød enklere konfigurasjonsfiler enn XML. Alternativer til YAML inkluderer JSON og TOML. For Elm, må du stole på JavaScript-biblioteker siden det er ingen direkte YAML-støtte. Når du bruker `ports`, send data mellom Elm og JavaScript forsiktig for å unngå runtime errors.

## See Also
- YAMLs offisiell nettsted: [yaml.org](https://yaml.org)
- `js-yaml` GitHub-side: [github.com/nodeca/js-yaml](https://github.com/nodeca/js-yaml)
- Elm ports: [guide.elm-lang.org/interop/ports](https://guide.elm-lang.org/interop/ports.html)
