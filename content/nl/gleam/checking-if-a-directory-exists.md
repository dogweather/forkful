---
title:                "Controleren of een directory bestaat"
date:                  2024-01-28T21:56:18.213936-07:00
model:                 gpt-4-0125-preview
simple_title:         "Controleren of een directory bestaat"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Controleren of een directory bestaat is net zoiets als even in een kamer gluren om te zien of hij er is voordat je erin stapt. Programmeurs doen dit om fouten te voorkomen bij het proberen te benaderen of wijzigen van directories die misschien niet aanwezig zijn.

## Hoe:

Om te controleren of een directory bestaat in Gleam, moeten we interactie hebben met het bestandssysteem. Helaas heeft Gleam, voor zover mijn kennis reikt in 2023, geen ingebouwde bestandssysteemoperaties omdat het voornamelijk is ontworpen voor het bouwen van fouttolerante systemen. We kunnen echter interface met de bestandssysteemoperaties van Erlang, dankzij het vermogen van Gleam om Erlang functies te gebruiken.

Hier is een snel voorbeeld met behulp van de `file` module van Erlang:

```Gleam
import gleam/erlang
import gleam/io

fn does_directory_exist(dir: String) -> Bool {
  case erlang.apply(
    module: "file", 
    function: "read_file_info", 
    args: [dir]
  ) {
    Ok(_) -> true
    Error(_) -> false
  }
}

fn main() {
  let directory = "/een/pad/naar/directory"
  laat exists = does_directory_exist(directory)
  io.println(exists)
}
```

Dit kan `true` uitvoeren als de directory bestaat, en `false` als het niet bestaat.

## Diepgaande discussie

Historisch gezien is Gleam jong en in ontwikkeling. Het is gebouwd op de BEAM virtuele machine, die Erlang gebruikt, en erft dus de robuuste functies van Erlang, inclusief bestandssysteemoperaties. Zonder native ondersteuning voor deze operaties, moeten we ons wenden tot Erlang-interoperabiliteit.

Alternatieven voor het controleren van het bestaan van directories zijn afhankelijk van het onderliggende systeem. In andere programmeertalen zijn er misschien directe aanroepen of standaardbibliotheken beschikbaar voor deze taken. Bijvoorbeeld, in Python zou je `os.path.isdir()` gebruiken.

Implementatiedetails met de functie `file.read_file_info` in Erlang vertellen ons dat het een tuple teruggeeft die de bestandsinformatie bevat als de operatie geslaagd is of een fouttuple als het mislukt is. Deze tuples kunnen dan worden gepatroontegeld om de uitkomst te bepalen. De succes tuple ziet eruit als `{:ok, FileInfo}` terwijl een fout wordt vertegenwoordigd als `{:error, Reden}`.

Hoewel het adopteren van Erlang-bibliotheken in Gleam momenteel de weg vooruit is voor taken als deze, is het de moeite waard om op te merken dat de gemeenschap in de toekomst mogelijk een speciaal pakket introduceert voor bestandssysteeminteracties.

## Zie ook

- [Erlang 'file' module documentatie](http://erlang.org/doc/man/file.html)
- [Erlang foutredenen voor bestandsoperaties](http://erlang.org/doc/apps/stdlib/io_protocol.html)
