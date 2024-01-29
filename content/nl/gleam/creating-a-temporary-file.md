---
title:                "Een tijdelijk bestand aanmaken"
date:                  2024-01-28T21:58:12.386274-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tijdelijk bestand aanmaken"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tijdelijk bestand creëren is precies wat het lijkt — het maken van een bestand voor kortetermijngebruik. Programmeurs doen dit voor taken zoals tijdelijke gegevensopslag of wanneer ze aan een bestand moeten werken zonder de originele inhoud te beïnvloeden.

## Hoe te:

Gleam heeft nog geen speciale standaardbibliotheek voor bestandsoperaties. In plaats daarvan vertrouwen we op Erlang interop om taken zoals het creëren van tijdelijke bestanden uit te voeren. Hier is hoe je dat doet:

```gleam
import gleam/erlang

fn main() {
  // Gebruikmaking van Erlang's :file module om een tijdelijk bestand te creëren
  case erlang.apply(
    module: ":file",
    function: "mktemp",
    arguments: ["./tmpFileXXXXXX"]
  ) {
    Ok(tuple) -> 
      let tmpPath = tuple.element(2)
      erlang.display(tmpPath) // Pad waar het tijdelijke bestand is aangemaakt
    Error(error_tuple) ->
      erlang.display(error_tuple)
  }
}
```

Wanneer je het programma uitvoert, krijg je een uitvoer vergelijkbaar met deze:

```plain
./tmpFileab3d5f
```

Dat is het pad naar je gloednieuwe tijdelijke bestand.

## Diepere duik

Historisch gezien is het werken met bestanden direct in Gleam een kwestie van het benutten van de onderliggende mogelijkheden van Erlang, waar Gleam naar compileert. Er kunnen externe Gleam-pakketten zijn die een meer idiomatiche, misschien veiligere manier bieden om met bestanden om te gaan, maar het hart en de ziel komen van Erlang's beproefde `:file` module.

Alternatieven zijn het gebruik van de capaciteiten van het besturingssysteem via shell-commando's, maar dit kan minder draagbaar zijn en lastiger te implementeren op een cross-platform compatibele manier. De ergonomie in de taal kan verbeteren naarmate het ecosysteem zich ontwikkelt — dus houd de opkomende bibliotheken in de gaten!

Tijdelijke bestanden zijn essentieel voor scenario's waarin je niet wilt dat gegevens langer bewaard blijven dan nodig, en ze helpen ook gegevenscorruptie te voorkomen tijdens risicovolle operaties. Bij het creëren van een tijdelijk bestand wordt gewoonlijk een unieke naam gegenereerd om het risico op naamconflicten te minimaliseren, en het wordt gedaan in een directory bedoeld voor tijdelijke opslag, wat ervoor zorgt dat het later wordt opgeruimd, hetzij door uw programma of door het OS.

## Zie ook

Voor meer gedetailleerde informatie over bestandsbehandeling in Gleam, bekijk deze links:

- [Erlang :file module Documentatie](http://erlang.org/doc/man/file.html)

Onthoud, Gleam is nog een beetje een groentje in de programmeerwereld, wat betekent dat zijn ecosysteem groeit. Houd [Hex](https://hex.pm/) (de pakketbeheerder voor het Erlang ecosysteem) in de gaten voor toekomstige bestandshandeling-pakketten die kunnen verschijnen!
