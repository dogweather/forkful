---
title:                "Fouten afhandelen"
date:                  2024-01-28T22:01:26.809167-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Fouten afhandelen draait om het anticiperen op zaken die fout kunnen gaan in je code en deze situaties op elegante wijze te beheren. Programmeurs doen dit omdat het applicaties robuust en gebruiksvriendelijk houdt, zelfs wanneer ze geconfronteerd worden met het onverwachte.

## Hoe te:
In Gleam maak je vaak gebruik van het `Result` type voor foutafhandeling. Het is een enum met twee varianten: `Ok` (voor succes) en `Error` (voor falen). Hier is een eenvoudig voorbeeld:

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("Oeps! Het is kapot.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(message) => {
      io.println(message)
      0
    } 
  }
}
```

Als je `main` uitvoert met `might_fail(False)`, zal het `42` teruggeven. Als je `True` doorgeeft, drukt het "Oeps! Het is kapot." af en retourneert het `0`.

## Diepgaande duik
Gleams benadering van foutafhandeling is beïnvloed door zijn Erlang-roots. Historisch gezien gebruikt Erlang een "laat het crashen" filosofie, waarbij op supervisiebomen wordt vertrouwd om procesfalen te beheren. Echter, wanneer je Gleam-code schrijft die niet binnen een te superviseren proces valt, zoals binnen een bibliotheekfunctie, wil je fouten expliciet afhandelen.

Alternatieven voor het gebruik van `Result` zijn onder andere het gebruik van het `Option` type voor gevallen waarin iets `None` (niets) of `Some` (iets) kan zijn, maar deze dragen geen foutinformatie over. Voor het signaleren van fouten over procesgrenzen heen, kun je gebruik maken van Erlangs berichtenoverdrachtsmechanismen.

Gleams foutafhandeling weerspiegelt een functionele programmeerstijl, waarbij bijwerkingen (zoals fouten) worden beheerd met types en patroonmatching, waardoor duidelijkheid en voorspelbaarheid in foutenbeheer wordt geboden.

## Zie ook
- [Erlangs foutafhandeling](http://erlang.org/doc/reference_manual/errors.html)
