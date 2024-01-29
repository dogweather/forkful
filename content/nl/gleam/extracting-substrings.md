---
title:                "Substrings extraheren"
date:                  2024-01-28T21:59:46.668579-07:00
model:                 gpt-4-0125-preview
simple_title:         "Substrings extraheren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Substringen extraheren is als het plukken van een kers van de taart; je pakt een specifiek stuk uit een tekenreeks. Programmeurs doen dit om kleinere brokken data te isoleren, analyseren of manipuleren uit grotere tekstsequenties.

## Hoe te:
Gleam heeft verschillende manieren om met het extraheren van substrings om te gaan. Hier volgen enkele voorbeelden:

```gleam
import gleam/string

let story = "Learning Gleam is fun!"

// "Gleam" uit de tekenreeks halen
let gleam = string.slice(story, 9, 14)
assert gleam == Ok("Gleam")

// "fun!" grijpen met behulp van een negatieve index
let fun = string.slice(story, -4, -1)
assert fun == Ok("fun!")

// Als de indices buiten de perken zijn, krijgen we een fout
let oops = string.slice(story, 30, 40)
assert oops == Error(Nil)
```
De `slice` functie is de go-to voor het krijgen van substrings. De opgegeven indices zijn inclusief aan het begin en exclusief aan het einde. Negatieve indices tellen vanaf het einde.

## Diepteonderzoek
Substring extraheren is niet nieuw; het is zo oud als de heuvels in programmering. In de goede oude tijd lieten talen zoals C je door hoepels springen met pointers om substrings te grijpen. Gleam en moderne talen vereenvoudigen de taak met ingebouwde functies, zoals `slice`.

Alternatieven? Zeker. Je zou patroonmatching kunnen gebruiken om strings te ontleden, maar `slice` is soepeler voor eenvoudige extracties.

Wat implementatie betreft, moet `slice` rekening houden met tekenreeksencodering, zoals UTF-8. Het zorgt ervoor dat karakters, niet slechts bytes, correct worden geëxtraheerd zonder meerbyte tekens te vervormen. Dit was geen pretje terug in het ASCII-only tijdperk.

## Zie ook
- Als je nostalgisch bent of gewoon nieuwsgierig, kijk dan hoe C omging met tekenreeksen met pointers: [C String behandeling](https://en.cppreference.com/w/c/string/byte)
