---
title:                "Logboekregistratie"
date:                  2024-01-28T22:02:47.643288-07:00
model:                 gpt-4-0125-preview
simple_title:         "Logboekregistratie"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Loggen is in wezen hoe we vastleggen wat er gebeurt in onze programma's. Het is als het hebben van een kleine zwarte doos; wanneer dingen fout gaan (en geloof me, dat zullen ze), zijn logs van onschatbare waarde om uit te zoeken wat er is gebeurd, problemen te diagnosticeren en de prestaties te optimaliseren.

## Hoe:
In Gleam zou je typisch een logbibliotheek gebruiken—er is geen speciaal logmechanisme kant-en-klaar beschikbaar. Laten we zeggen dat we een hypothetische `gleam_logger` crate gebruiken. Zo zou je het kunnen integreren:

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("App start op!")
  let resultaat = intense_berekening()

  case resultaat {
    Ok(waarde) -> 
      gleam_logger.debug("Berekening succesvol", waarde)
    Error(err) -> 
      gleam_logger.error("Berekening mislukt", err)
  }
}
```

Verwachte uitvoer in je logs zou er ongeveer zo uitzien:

```
INFO: App start op!
DEBUG: Berekening succesvol 42
ERROR: Berekening mislukt Reden: Delen door nul
```

## Diepgaand
De kunst van het loggen bestaat al sinds de vroege dagen van het programmeren. Systeembeheerders kregen letterlijk logs van de computer - om ervoor te zorgen dat alles soepel verliep. Fast forward, en loggen is digitaal geworden, een kernonderdeel van softwareontwikkeling.

Hoewel Gleam, als een relatief jonge taal die zich richt op het Erlang-ecosysteem, geen ingebouwd logkader heeft, kun je de volwassen Erlang-logfaciliteiten of andere door de gemeenschap geleverde bibliotheken benutten. Elk heeft verschillende functies en afwegingen: sommige kunnen gestructureerd loggen bieden, andere zijn meer voor eenvoudige tekstuitvoer.

Nu, de vraag van het implementeren van een logfaciliteit: Is het eenvoudig? Op het eerste gezicht, ja. Maar als je de lagen terugschilt, kijk je naar het omgaan met concurrentie, I/O knelpunten, logrotatie, standaardisatie van formaat (denk aan JSON voor gestructureerd loggen), niveau filtering en mogelijk gedistribueerde tracing. Bovendien wil je in een functioneel paradigma over het algemeen bijeffecten (zoals loggen) op een voorspelbare en gecontroleerde manier afhandelen.

## Zie Ook
Hier vind je meer over de ins en outs van loggen in Gleam en het omliggende ecosysteem:
- [Erlang's :logger documentatie](http://erlang.org/doc/apps/kernel/logger_chapter.html): Aangezien Gleam compileert naar Erlang, is dit direct toepasbaar.
- [Gleam's standaardbibliotheek docs](https://hexdocs.pm/gleam_stdlib/): Voor updates over eventuele loghulpmiddelen die kunnen worden toegevoegd.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam): Een samengestelde lijst van bronnen, die logbibliotheken kan bevatten naarmate ze beschikbaar komen.
