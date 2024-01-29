---
title:                "Een datum uit een string parsen"
date:                  2024-01-28T22:04:19.703473-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum uit een string parsen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een datum uit een tekenreeks halen gaat over het omzetten van tekst naar een datumformaat dat je programma begrijpt. Programmeurs doen dit om datums en tijden effectief te beheren, zoals het sorteren van evenementen of het plannen van taken.

## Hoe te:

In Gleam is er, voor zover ik weet tot begin 2023, geen ingebouwde manier om datums uit strings te parsen. Normaal gesproken zou je in andere talen een bibliotheek zoals `chrono` of `time` gebruiken. Je hebt mogelijk een externe bibliotheek of een aangepaste functie nodig om de klus in Gleam te klaren. Hier is een basisvoorbeeld met een hypothetische `parse_date` functie.

```gleam
import gleam/calendar.{Datum}

fn parse_date(datum_string: String) -> Result(Datum, String) {
  // Hier zou je je logica voor het parsen van datums implementeren.
  // Laten we voor nu doen alsof het magisch werkt.
  Ok(Datum(jaar: 2021, maand: 3, dag: 14))
}

pub fn main() {
  let datum_string = "2021-03-14"
  case parse_date(datum_string) {
    Ok(datum) -> 
      datum
    Error(error) ->
      error
  }
}
// Voorbeelduitvoer: Datum(jaar: 2021, maand: 3, dag: 14)
```

## Diepere Duik

Het vermogen om datums te parsen komt voort uit de behoefte om op een gestandaardiseerde manier met datums te interageren. Vroege programmeurs gebruikten verschillende formaten, wat tot verwarring leidde. Standaarden zoals ISO 8601, die datums in het formaat JJJJ-MM-DD voorstellen, hielpen bij het uniformeren van de datumsrepresentatie over systemen heen.

Zonder ingebouwde ondersteuning in Gleam voor het parsen van datums, heb je twee keuzes: grijp naar een externe bibliotheek of maak je eigen oplossing. Bij het schrijven van je eigen parser, overweeg randgevallen en houd je aan een standaardformaat voor consistentie.

Wat betreft prestaties kan het parsen duur zijn. Om dit te verbeteren, kun je veelvoorkomende datumpatronen vooraf verwerken. In gedistribueerde systemen, zorg ervoor dat de geparseerde datums voldoen aan de verwachte tijdzones en locales, aangezien interpretaties van datums kunnen variëren.

## Zie ook

Hoewel er op dit moment geen officiële Gleam-bibliotheken voor datum-parsing zijn, kan het kijken naar hoe andere talen dit aanpakken inspiratie bieden. Bekijk deze eens:

- Rust: [chrono](https://docs.rs/chrono)
- Python: [dateutil](https://pypi.org/project/python-dateutil/)
- Elm: [elm/time](https://package.elm-lang.org/packages/elm/time/latest/)
