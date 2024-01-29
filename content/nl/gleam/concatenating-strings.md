---
title:                "Samenvoegen van strings"
date:                  2024-01-28T21:56:43.881499-07:00
model:                 gpt-4-0125-preview
simple_title:         "Samenvoegen van strings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het samenvoegen van strings is het aan elkaar plakken van twee of meer strings van begin tot eind om een nieuwe te maken. Programmeurs doen dit om zinnen te vormen, dynamische gegevens met tekst te mengen of patronen te creëren voor programmeerelegantie.

## Hoe:

Direct naar de code, hier is hoe je de tango danst met strings in Gleam:

```gleam
fn main() {
  let begroeting = "Hallo"
  let onderwerp = "Wereld"
  let uitroepteken = "!"

  let bericht = begroeting ++ " " ++ onderwerp ++ uitroepteken
  bericht
}

// Verwachte uitvoer: "Hallo Wereld!"
```

Kinderspel, toch? Sla gewoon strings aan elkaar vast met `++` en je hebt een stringstoofpotje.

## Diepere Duik

Concatenatie lijkt eenvoudig, maar er is veel onder de motorkap. Historisch gezien kon stringconcatenatie in programmeertalen ingewikkeld worden met verschillende typen of onveranderlijkheidsproblemen. Alternatieven zijn stringformatting of bouwen met lijsten, maar concatenatie blijft een voorkeur vanwege de eenvoud.

In Gleam, dat zuiverheid en sterke typen hoog in het vaandel draagt, gebruikt stringconcatenatie de `++` operator die ervoor zorgt dat de typen juist zijn en het resultaat elke keer een nieuwe string is - hier geen bijwerkingen.

## Zie Ook

Voor meer string-gebaseerde capriolen:

- Introductie tot Gleam: [https://gleam.run](https://gleam.run)
