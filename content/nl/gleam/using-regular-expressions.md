---
title:                "Reguliere expressies gebruiken"
date:                  2024-01-28T22:09:46.959549-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguliere expressies gebruiken"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Reguliere expressies (regex) zijn patronen die worden gebruikt om karaktercombinaties in strings te matchen. Programmeurs gebruiken ze voor zoeken, valideren en manipuleren van tekst omdat ze snel en efficiënt zijn.

## Hoe te:

Gleam heeft geen ingebouwde regex-ondersteuning, dus we gebruiken de `gleam_otp/re` module. Hier is de procedure:

Voeg eerst `gleam_otp` toe aan je `rebar.config` afhankelijkheden.

```erlang
{deps, [
    {gleam_otp, "0.1.0"}
]}
```

Schrijf nu wat Gleam-code om patronen te matchen:

```rust
import gleam/otp/re

fn main() {
  let pattern = "^Hello, (\\w+)!"
  let text = "Hello, World!"
  
  let result = re.run(pattern, text)
  jsonecho(result)
}

fn jsonecho(result) {
  case result {
    Ok(matches) -> 
      case matches {
        [] -> 
          "Geen match gevonden"
        [_, naam] -> 
          "Match gevonden met naam: " <> naam
        _ -> 
          "Onverwachte match-aantal"
      }
    Error(_) -> 
      "Patroon compileerde niet"
  }
}
```

Voer het uit, en je ziet `Match gevonden met naam: World`.

## Diepgaande duik

Regex bestaat al sinds de jaren '50; het gebruik ervan in Unix-tools in de jaren '70 bevestigde zijn plaats. Alternatieven omvatten string-functies, maar die kunnen langdradig zijn. Qua implementatie compileert regex meestal naar een interne staatmachine, wat snelheid biedt maar potentieel complexiteit in het schrijven van complexe patronen.

## Zie ook

- [Regex101](https://regex101.com/) voor het online testen van regex-patronen.
- [Introductie tot Reguliere Expressies](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) voor een fundamentele basis.
