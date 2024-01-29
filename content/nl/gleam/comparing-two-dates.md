---
title:                "Twee datums vergelijken"
date:                  2024-01-28T21:56:34.054300-07:00
model:                 gpt-4-0125-preview
simple_title:         "Twee datums vergelijken"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het vergelijken van twee datums is controleren hoe deze zich in tijd tot elkaar verhouden. Programmeurs doen dit om evenementen te sorteren, schema's te implementeren of perioden te valideren.

## Hoe te:

In Gleam gebruiken we de `gleam/calendar` bibliotheek voor datumafhandeling. Helaas, voor zover mijn kennis reikt in begin 2023, heeft Gleam geen ingebouwde manier om direct datums te vergelijken zoals sommige andere talen dat misschien wel hebben. Dus, hoewel dit niet plug-and-play is, kunnen we met een paar functies datums gaan vergelijken.

Laten we eerst zorgen dat we wat datums kunnen maken:

```gleam
import gleam/calendar.{Date}

pub fn make_date(year: Int, month: Int, day: Int) -> Option(Date) {
  calendar.new_date(year, month, day)
}
```

Nu, laten we een functie schrijven om twee datums te vergelijken. We kunnen datums converteren naar een vergelijkbaar formaat - zoals het aantal dagen sinds een bepaalde datum. Maar aangezien dit een eenvoudig voorbeeld is, laten we gewoon een basiscontrole doen om te zien of de ene datum voor de andere is:

```gleam
import gleam/calendar.{Date, is_before}

pub fn is_date1_before_date2(date1: Date, date2: Date) -> Bool {
  is_before(date1, date2)
}
```

Voorbeeldgebruik:

```gleam
import gleam/io

fn main() {
  let date1 = make_date(2023, 3, 14)
  let date2 = make_date(2021, 6, 18)
  
  let resultaat = case date1 {
    Ok(d1) -> case date2 {
      Ok(d2) -> is_date1_before_date2(d1, d2)
      Error(_) -> False
    }
    Error(_) -> False
  }
  
  io.debug(resultaat) // Zou True moeten afdrukken, want date1 is na date2
}
```

## Diepere Duik

Historisch gezien variëren datum/tijd API's over talen, waarbij sommige robuuste vergelijkingsoperators bieden en andere handmatige berekeningen vereisen. Bij het vergelijken van datums converteren veel talen naar een gestandaardiseerde vorm zoals Unix-tijd (seconden sinds 1 januari 1970), die direct kan worden vergeleken. Echter, randgevallen zoals schrikkelseconden of zomertijd kunnen complexiteit toevoegen.

In Gleam, vanwege de focus van de taal op veiligheid en betrouwbaarheid, kunnen datumoperaties minder rechttoe rechtaan zijn maar streven ze ernaar juist te zijn zonder impliciete aannames. Daarom vind je misschien geen one-liner om dit soort werk te doen, maar met de juiste afhandeling van datums met de `calendar` module, kun je het goed beheren.

Als alternatief kan men complexere functies schrijven die het jaar, dan de maand, dan de dag vergelijken, als er behoefte is aan fijnmazige controle of totdat ondersteuning voor directe datumsvergelijking daadwerkelijk aan Gleam wordt toegevoegd. Ten slotte, houd altijd de taalupdates in de gaten; Gleam evolueert snel en nieuwe functies kunnen beschikbaar komen na mijn kennisdatum.

## Zie Ook

- Voor de volledige Gleam taal tutorial, bezoek: [https://gleam.run](https://gleam.run).
- Voor dilemma's over tijd- en datumafhandeling in programmeren en hun oplossingen, lees [https://yourcalendricalfallacyis.com/](https://yourcalendricalfallacyis.com/).
