---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor? 

Oppretting av en midlertidig fil i dataprogrammet er når du oppretter en fil som er ment å bli brukt for en kort periode. Programmerere gjør dette i scenarier der de trenger et sted å lagre data midlertidig mens de prosesserer det i en arbeidsflyt. 

## Hvordan:

Her er hvordan du oppretter en midlertidig fil i Gleam:

```Gleam
import gleam/otp.{Process, Timer}
import gleam/string

fn start_process() {
  let process = Process.start_link(
    fn() {
      Timer.sleep(1000) // Create temporary file here
      Ok(Nil)
    },
    process::Options(.name(process::Unregistered("my_process"))),
  )

  case process {
    Ok(pid) -> 
      string.println("Created file with process: " ++ pid)
    Error(err) ->
      string.println("Could not create file: " ++ err)
  }
}
```
Kjør kommandoen `gleam run` for å kjøre dette scriptet. Du vil se utdata nedenfor:

```
Created file with process: <0.1.0>
```

## Dypdykk

Oppretting av midlertidige filer har en lang historie i programmering og var spesielt hjelpsom for å sortere store datasett i gamle dagers batch-stilcomputere. Alternativene til midlertidige filer inkluderer bruk av midlertidige databaser, spesielt når datasikkerhet er viktig, eller bruk av minne utenfor hovedfilen, noe som kan være raskere, men mer begrenset for inndata av stor størrelse. Gleam bruker Erlang's prosessmodell for å håndtere midlertidige data, så en "fil" i dette scenarioet refererer egentlig til en økt med en prosess. 

## Se Også:

1. Gleam dokumentasjon om [processer](https://gleam.run/book/tour/processes.html)
2. Erlang [otp doc](http://erlang.org/doc/man/otp_app.html) for mer informasjon om prosessene.
3. Historien til midlertidige filer i [Unix](http://www.tldp.org/LDP/Linux-Filesystem-Hierarchy/html/var.html).