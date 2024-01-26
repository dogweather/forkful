---
title:                "Skriving av en tekstfil"
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving til tekstfiler lar oss lagre data på en varig måte. Programmerere gjør dette for å logge hendelser, lagre konfigurasjoner, eller for å kommunisere med andre programmer eller systemkomponenter.

## Hvordan:
Gleam har ikke innebygde biblioteker for filoperasjoner ennå, men du kan integrere med Erlang-biblioteker ved å bruke `gleam_erlang` bindingene. Eksempel:

```gleam
import gleam/io
import gleam_erlang.{File}
import gleam/ok.{Ok, Error}

fn write_to_file(content: String) {
  case File.open("my_file.txt", [:write]) {
    Ok(file) ->
      File.write(file, content)
      io.println("File written successfully")
      File.close(file)
    Error(error) ->
      io.println("Failed to write file: " ++ error)
  }
}
```

Kjører du funksjonen vil outputten i terminalen være enten "File written successfully" eller "Failed to write file: ...med feilmelding...".

## Deep Dive:
Før i tiden brukte vi biblioteket `:file` fra Erlang direkte, men `gleam_erlang` gir en bedre, typestøttet integrasjon med Erlang's funksjonalitet. Alternativer til `gleam_erlang` kunne være direkte systemkall eller bruk av andre språkmoduler. Når du skriver til filer skal du passe på å håndtere feil som kan oppstå og lukke filresursen for å unngå ressurslekkasje.

## See Also:
- Gleam's offisielle dokumentasjon: https://gleam.run/
- Erlang's `:file` moduldokumentasjon: http://erlang.org/doc/man/file.html
- Gleam's `gleam_erlang` bibliotek: https://hex.pm/packages/gleam_erlang
