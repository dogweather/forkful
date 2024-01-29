---
title:                "Een datum in de toekomst of het verleden berekenen"
date:                  2024-01-28T21:55:39.900246-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum in de toekomst of het verleden berekenen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het berekenen van een toekomstige of verleden datum betekent uitzoeken wat de datum zal zijn na of voor een bepaalde tijd. Programmeurs doen dit voor zaken zoals het plannen van evenementen, het verlengen van abonnementen, of het instellen van vervaldatums.

## Hoe:
Gleam heeft geen ingebouwde datum-/tijdbibliotheek, dus we gebruiken de `chronotope` bibliotheek om data en tijden te beheren. Voeg eerst `chronotope` toe aan je `gleam.toml`:

```toml
[dependencies]
chronotope = "~> 0.4"
```

Laten we nu wat datumcalculaties uitvoeren:

```gleam
import chronotope
import chronotope.duration
import chronotope.date

fn calculate_date() {
  let now = chronotope.now()
  let two_weeks = duration.of_weeks(2)
  let future_date = date.add(now, two_weeks)
  let past_date = date.subtract(now, two_weeks)
  future_date, past_date
}

fn main() {
  let (future, past) = calculate_date()
  io.println(future)
  io.println(past)
}
```

Voer het uit:

```bash
$ gleam run
```

Voorbeelduitvoer zou kunnen zijn:

```
2023-04-28
2023-03-31
```

## Diepere duik
In de informatica maakt datummanipulatie deel uit van het bredere veld van temporele databases en tijdgebaseerde data. In de jaren '70, met mainframecomputers als hoofdmoot, was nauwkeurige datum- en tijdregistratie al essentieel voor functies zoals taakplanning.

Wat betreft alternatieven, hoewel `chronotope` een solide keuze is in Gleam, kunnen andere talen standaardbibliotheken gebruiken zoals Python's `datetime` of het `Date` object van JavaScript. De implementatie verschilt per taal, maar meestal berekenen ze datums door milliseconden te manipuleren sinds een bekend tijdperk (gewoonlijk 1 januari 1970, UTC).

Intern beheert `chronotope` datums als structuren en voert berekeningen uit door intervallen om te zetten naar een compatibele eenheid (bijvoorbeeld seconden of dagen), er wiskunde op toe te passen, en het vervolgens terug te converteren naar een datum- of tijdstructuur. Dit proces houdt rekening met grillen in kalenders en tijdzones, die niet altijd lineair of consistent zijn vanwege schrikkeljaren, zomertijd en andere anomalieën.

## Zie ook
- [Temporele databases op Wikipedia](https://nl.wikipedia.org/wiki/Temporele_database)
- [Geschiedenis van tijdmeetapparaten](https://nl.wikipedia.org/wiki/Geschiedenis_van_het_uurwerk)
