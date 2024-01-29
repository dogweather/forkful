---
title:                "Een tekstbestand lezen"
date:                  2024-01-28T22:05:20.528526-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand lezen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tekstbestand lezen betekent het inlezen van gegevens uit een bestand dat bestaat uit tekst op je schijf. Programmeurs doen dit om toegang te krijgen tot en informatie te manipuleren, zoals configuraties, logs, of welke gegevens hun apps ook nodig hebben.

## Hoe te:
Gleam bevat geen file IO in zijn standaard bibliotheek, dus we zullen Erlang-functies gebruiken. We beginnen met het openen van een tekstbestand met `file.open/2`, lezen de inhoud ervan, behandelen mogelijke fouten en sluiten ten slotte het bestand. Hier is de essentie:

```gleam
import gleam/erlang
import gleam/result

fn main() {
  case erlang.file.open("voorbeeld.txt", [read]) {
    Ok(file) ->
      case erlang.file.read(file) {
        Ok(data) -> {
          erlang.io.format("Inhoud: ~p~n", [data])
          erlang.file.close(file)
        }
        Error(err) -> {
          erlang.io.format("Fout bij het lezen van bestand: ~p~n", [err])
        }
      }
    Error(err) ->
      erlang.io.format("Fout bij het openen van bestand: ~p~n", [err])
  }
}
```

Voer dit uit en je ziet de inhoud van je tekstbestand, of een fout als er iets misging.

## Diepere duik
Bestanden lezen is niets nieuws; het is al in de programmering sinds de dagen van ponskaarten. Gleam, een statisch getypeerde taal die compileert naar de Erlang VM, leunt op Erlang's volwassen ecosysteem voor bestandsbewerkingen. Je hebt ook andere opties: asynchrone lezingen, regels streamen, of bibliotheken gebruiken zoals `gleam_otp` voor een meer Gleam-achtige aanpak.

Het begrijpen van file IO omvat foutafhandeling. Bestanden bestaan misschien niet, kunnen vergrendeld zijn, of je hebt misschien geen toestemming. Gleam's patroonmatching en `result` module geven je een duidelijk pad voor het beheren van het onverwachte.

Ten slotte, overweeg de grootte van je bestand. Onze simpele `erlang.file.read` leest het hele ding in het geheugen, wat problematisch kan zijn voor enorme bestanden. Het streamen van brokken of regels zou efficiënter zijn.

## Zie ook
- [Erlang's file module documentatie](http://erlang.org/doc/man/file.html) aangezien we Erlang's mogelijkheden gebruiken.
- [Erlang's IO documentatie](http://erlang.org/doc/apps/stdlib/io_protocol.html) voor het begrijpen hoe input/output werkt onder de motorkap.
