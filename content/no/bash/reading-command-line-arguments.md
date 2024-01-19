---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Kommandolinje-argumenter er inndata som styres via terminalet når du kjører et skript. De hjelper programmerere med å øke fleksibiliteten og tilpasningsevnen til kodeskriptene sine.

## Slik gjør du:

Å lese kommandolinje-argumenter i Bash er enkelt. Argumentene blir lagret i spesielle variabler ($0,$1,$2,...). Her er et eksempel:

```Bash
#!/bin/bash
echo "Skriptnavnet er $0"
echo "Første argument er $1"
echo "Andre argument er $2"
```

Bruk skriptet slik: `./skript.sh arg1 arg2` . Og output blir:

```
Skriptnavnet er ./skript.sh
Første argument er arg1
Andre argument er arg2
```

## Deep Dive

Kommandolinje-argumenter i Bash har røtter tilbake til de tidlige dagene av UNIX. De gir brukeren mulighet til å påvirke skriptenes oppførsel uten å måtte endre koden.

Alternativene til dem inkluderer bruk av inputstrømmer eller filinput/output, men ingen av dem gir samme nivå av brukervennlighet og enkel tilpasning.

Husk at antall argumenter er lagret i `$#`, og samling av alle argumentene er tilgjengelig i `$@`. `shift` kommandoen kan brukes til å 'rotete' argumentene til venstre.

## Se også:

- 'Command Line Parameters in Shell Script' på TutorialsPoint: https://www.tutorialspoint.com/unix/unix-command-line.htm
- 'Special Parameters' i Bash manual: https://www.gnu.org/software/bash/manual/bash.html
- 'Input and Output' i Advanced Bash-Scripting Guide: http://www.tldp.org/LDP/abs/html/io-redirection.html