---
title:                "Een datum converteren naar een string"
date:                  2024-01-28T21:57:42.675927-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum converteren naar een string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een datum omzetten naar een tekenreeks betekent het omvormen van een datumobject, dat een specifiek moment in de tijd voorstelt, naar een door mensen leesbaar tekstformaat. Programmeurs doen dit om datums op een gebruiksvriendelijke manier weer te geven of om ze te serialiseren voor opslag en communicatie.

## Hoe te:

In Gleam is er geen ingebouwd datatypetype, maar laten we aannemen dat we een aangepast `Date`-type gebruiken en dat we het willen converteren naar een tekenreeks. Definieer eerst uw datumtype en conversiefunctie:

```gleam
type Date {
  Date(year: Int, month: Int, day: Int)
}

fn date_to_string(date: Date) -> String {
  let Date(year, month, day) = date
  int_to_string(year) ++ "-" ++ int_to_string(month) ++ "-" ++ int_to_string(day)
}

pub fn main() {
  let my_date = Date(2023, 4, 3)
  let date_string = date_to_string(my_date)
  io.println(date_string) // "2023-4-3"
}
```

## Diepgaande duik

Historisch gezien zijn datumformattering en -parsing complex geweest vanwege verschillende datum- en tijdrepresentaties in diverse locales en standaarden. De meeste programmeeromgevingen bieden bibliotheken die deze complexiteiten aanpakken. In Gleam, dat streeft naar sterke typeveiligheid en gelijktijdigheid, heb je vaak te maken met externe bibliotheken, zoals `chronotope`, voor datum-tijdbewerkingen.

Een alternatief voor handmatige tekenreeksconversie is het gebruiken van een gestandaardiseerd formaat zoals ISO 8601 (`YYYY-MM-DD`), wat geïmplementeerd kan worden met functies die enkelcijferige maanden en dagen opvullen met nullen.

Wat betreft de implementatie, kan het omzetten van datum naar tekenreeks meer inhouden dan het aaneenschakelen van gehele getallen; locatiespecifieke voorkeuren kunnen het gebruik van schuine strepen of punten in plaats van streepjes dicteren, en er is ook de kwestie van tijdzones en of men tijdinformatie naast de datum moet opnemen.

## Zie ook

- Het Gleam Boek: https://gleam.run/book/
- `chronotope` crate (indien beschikbaar voor de huidige versie van Gleam): [Link naar crate-documentatie]
- ISO 8601 Datum- en Tijdformaat: https://www.iso.org/iso-8601-date-and-time-format.html
