---
title:                "Arbeiten mit TOML"
aliases:
- de/elm/working-with-toml.md
date:                  2024-01-26T04:21:06.635750-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-toml.md"
---

{{< edit_this_page >}}

## Was & Warum?
TOML, die Abkürzung für Toms Offensichtliche, Minimale Sprache, ist eine Daten-Serialisierungssprache. Elm-Programmierer verwenden sie zur Verwaltung von Konfigurationsdaten, da sie menschenlesbar ist und sich sauber auf die für Anwendungen benötigten Schlüssel-Wert-Paare abbilden lässt.

## Wie:
Elm hat keinen eingebauten TOML-Parser, aber Sie können eine Interoperabilität mit JavaScript nutzen oder ein Community-Paket verwenden. So könnte das Parsen von TOML mit einem hypothetischen `elm-toml`-Paket funktionieren:

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

Für das Dekodieren spezifischer Werte:

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

Ein Beispieloutput für `port` könnte `Ok 8080` sein, wenn das Dekodieren erfolgreich ist.

## Tiefere Einblicke
TOML wurde von Tom Preston-Werner, dem Mitbegründer von GitHub, als einfache Sprache für Konfigurationsdateien erstellt. Es konkurriert mit YAML und JSON; TOMLs Syntax zielt darauf ab, das Beste aus beiden Welten zu bieten mit einem Fokus darauf, leicht für Menschen les- und schreibbar zu sein.

In Elm müssen Sie sich typischerweise der Interoperabilität mit JavaScript bedienen, was etwas mühsam sein kann. Glücklicherweise ist die Elm-Community findig, und es existieren mehrere Drittanbieter-Pakete. Das hypothetische `elm-toml`-Paket würde wahrscheinlich Elms `Port` nutzen, um mit einem JavaScript-TOML-Parser zu kommunizieren oder das Parsen direkt in Elm zu implementieren.

Die Hauptschwierigkeit in Elm besteht darin, dass alles statisch typisiert wird, sodass Sie benutzerdefinierte Decoder schreiben müssen, um unterschiedliche Datenstrukturen innerhalb von TOML zu behandeln, was etwas umständlich sein kann, aber Sicherheit hinzufügt.

## Siehe Auch
Für Spezifikationen und weitere Informationen zu TOML selbst, besuchen Sie [TOML](https://toml.io).
Wenn Sie einen praktischen Ansatz zur Interoperabilität von Elm und JavaScript suchen, beginnen Sie mit dem offiziellen Leitfaden: [Elm Ports](https://guide.elm-lang.org/interop/ports.html).
Um Community-Pakete anzusehen oder beizutragen, durchstöbern Sie [Elm Packages](https://package.elm-lang.org/).
