---
title:                "Tests Schrijven"
date:                  2024-01-28T22:12:48.116008-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schrijven"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het schrijven van tests gaat over het creëren van code die de correctheid van andere code controleert. Programmeurs doen dit om vroegtijdig fouten op te sporen, kwaliteit te waarborgen en te voorkomen dat toekomstige wijzigingen dingen kapot maken.

## Hoe:

```Gleam
import gleam/should
import my_module

pub fn mijn_test() {
  // Controleren of de functie de verwachte waarde retourneert
  should.equal(my_module.my_function(), "verwacht resultaat")
}

pub fn optel_test() {
  // De optelfunctie testen op correctheid
  should.equal(my_module.add(1, 2), 3)
}
```

Voorbeelduitvoer van een succesvolle testreeks:

```
Testing my_module...
  ✓ mijn_test slaagt
  ✓ optel_test slaagt

Alle tests geslaagd!
```

## Diepgaande Duik

De testcultuur van Gleam is geïnspireerd door zijn Erlang-roots, waar robuustheid essentieel is. Alternatieven zoals op eigenschappen gebaseerd testen zijn ook populair in het Erlang-ecosysteem. Wat implementatie betreft, zijn tests in Gleam gewoon reguliere functies met beweringen. Ze worden uitgevoerd door een testrunner en resultaten worden gerapporteerd in een voor mensen leesbaar formaat.

## Zie Ook

- Erlang's common test for context: [http://erlang.org/doc/apps/common_test/basics_chapter.html](http://erlang.org/doc/apps/common_test/basics_chapter.html)
