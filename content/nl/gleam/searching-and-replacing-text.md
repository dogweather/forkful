---
title:                "Tekst zoeken en vervangen"
date:                  2024-01-28T22:08:01.412385-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekst zoeken en vervangen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Tekst zoeken en vervangen houdt in dat specifieke strings binnen een grotere tekst gezocht worden en verwisseld worden met alternatieve strings. Programmeurs doen dit voor taken zoals het bijwerken van code, het corrigeren van fouten of het verwerken van gegevensformaten.

## Hoe:

In Gleam houden we het eenvoudig. Je wilt zoeken naar "world" en dit vervangen door "Gleam". Hier is de code:

```gleam
import gleam/string

pub fn replace_world_with_gleam(input: String) -> String {
  string.replace(input, "world", "Gleam")
}

pub fn main() {
  let text = "Hello, world!"
  laat nieuwe_tekst = replace_world_with_gleam(text)
  nieuwe_tekst
}
```

Voorbeeld output:

```
"Hello, Gleam!"
```

Geen gedoe, werkt als een zonnetje.

## Diepere Duik

Historisch gezien is tekst zoeken en vervangen zo oud als de weg naar Rome in programmeertermen. Het is basaal maar essentieel, zoals schroevendraaiers voor een timmerman. In de functionele familie gaat het ons om strings zonder bijwerkingen. In Gleam is de `string` module je beste vriend, het is ingebouwd en klaar voor gebruik. Geen wiel opnieuw uitvinden hier.

Alternatieven? Zeker, je zou regex kunnen gebruiken als je behoeften complex worden, of misschien duiken in verschillende bibliotheken of talen, maar voor veel taken slaat Gleam's `string.replace` de spijker op zijn kop. Het is netjes, functioneel (Gleam is een sterke, statisch getypeerde taal in de ML-familie), en het integreert in het BEAM-ecosysteem - net als Erlang en Elixir.

Wanneer je `string.replace` uitvoert, activeer je een reeks karakterpatroonmatching onder de motorkap. Het is efficiënt en doet het werk zonder dat je je zorgen hoeft te maken over de fijne details. Gleam's leidende principe is om dingen type-veilig en bondig te houden - dat geldt ook voor stringmanipulatie.

## Zie Ook

- Documentatie van de Gleam String-module: https://hexdocs.pm/gleam_stdlib/gleam/string/
- Regelmatige expressies in Gleam met de `gleam_regex` bibliotheek: https://hex.pm/packages/gleam_regex
- Voor een groter beeld, zie de algemene filosofie van Gleam: https://gleam.run/book/tour/philosophy.html

Of je taak nu eenvoudig of complex is, Gleam ondersteunt je voor alles wat met strings te maken heeft. Gelukkig coderen!
