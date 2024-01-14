---
title:    "Bash: Lesing av kommandolinjeargumenter"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor
Hvis du er en programmør som jobber med Bash, vet du sannsynligvis allerede at å lese kommandolinjeargumenter er en viktig del av å få et program til å fungere som det skal. Men hvis du er ny til Bash, eller kanskje bare lurer på hva dette betyr, frykt ikke! Vi skal gå gjennom det grunnleggende ved å lese og behandle kommandolinjeargumenter, og hvorfor dette er nyttig.

## Hvordan
For å lese kommandolinjeargumenter i Bash, bruker vi et spesielt variabelformat: *$n*, der *n* er et tall. Her representerer *$0* navnet på programmet som blir utført, *$1* er den første kommandolinjeargumenten, *$2* er den andre, og så videre. La oss se på et enkelt eksempel:

```Bash
#!/bin/bash

echo "Hei, $0. Du skrev inn $# argumenter:"

for arg in "$@"; do
  echo "- $arg"
done
```

I dette eksemplet, hvis navnet på filen er *lesargs.sh*, og vi kjører kommandoen `./lesargs.sh Hallo verden`, vil output bli:

```
Hei, lesargs.sh. Du skrev inn 2 argumenter:
- Hallo
- verden
```

I kodesnutten over bruker vi `$#` for å vise antall argumenter som blir gitt, og `$@` for å iterere gjennom alle argumentene. Merk at vi bruker anførselstegn rundt `$@`, slik at argumenter med mellomrom blir håndtert riktig.

## Dypdykk
I tillegg til å lese argumenter, kan vi også behandle dem etter behov. Dette kan inkludere å validere argumenter og håndtere feil, eller parse dem for å hente ut spesifikk informasjon. Det er også mulig å bruke flagg i tillegg til argumenter, ved hjelp av *getopts* kommandoen.

En annen viktig ting å huske på er at rekkefølgen på argumenter kan være viktig for noen programmer. Det kan være lurt å bruke *sort -n* for å sortere argumentene numerisk, eller *sort -M* for å sortere datoen.

## Se også
- [Bash-tilpassede kommandoer for håndtering av kommandolinjeargumenter](https://www.thegeekstuff.com/2013/10/read-bash-arguments/)
- [Unix-tutorial: Kommandolinjeargumenter](https://www.tutorialspoint.com/unix/unix-command-line-arguments.htm)
- [Bash-dokumentasjon: Handtering av kommandolinjeargumenter](https://www.gnu.org/software/bash/manual/html_node/Command-Line-Arguments.html)