---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese en tekstfil er prosessen med å hente data, vanligvis kodet i ASCII eller Unicode, fra en fil for bruk i et program. Programmet leser det for å få tak i informasjonen den inneholder, som kan være input for beregninger, innstillinger eller noe annet.

## Hvordan gjør du det:

Her er en grunnleggende kode for å lese en tekstfil i Gleam:

```Gleam
import gleam/stdio
import gleam/bit_builder.{BitBuilder}
import gleam/file

pub fn main(file_path: String) {
    case file.read(file_path) {
    Ok(file_contents) ->
        stdio.print(file_contents)
    Error(e) ->
        stdio.print(e)
    }
}
```

Hvis filen "test.txt" inneholder teksten "Hallo, Verden!", vil utgangen være:

```Gleam
"Hello, Verden"
```

## Dyp Dykk:

Histørisk sett har fillesing alltid vært en nøkkelkomponent i programmering. Før utbredelsen av databaser, var filer den primære måten å lagre og hente data på.

Som alternativer kan dataene leses fra andre kilder, som en database, over nettverket, eller til og med genereres dynamisk.

Fra implementeringsdetaljene er `file.read()` en blokkerende operasjon i Gleam, hvilket betyr at Gleam venter til hele filen leses før den går videre. Hvis filen er stor, kan dette føre til ytelsesproblemer.

## Se også:

[Gleam Dokumentasjon](https://gleam.run/docs/)
[Gleam Eksempel Kode](https://github.com/gleam-lang/gleam)
[Gleam Filhåndtering](https://hexdocs.pm/gleam_stdlib/gleam/file/document.html)