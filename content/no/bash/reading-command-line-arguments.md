---
title:    "Bash: Lesing av kommandolinje-argumenter"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en programmerer eller en systemadministrator, er det stor sjanse for at du allerede er kjent med Bash-programmeringsspråket. En av de nyttigste funksjonene i Bash er muligheten til å lese kommandolinje-argumenter, som tillater deg å kjøre et skript eller et program med ulike innstillinger og variabler. I denne bloggposten vil vi se nærmere på hvorfor det er viktig å lære å lese kommandolinje-argumenter og hvordan man gjør det.

## Hvordan gjøre det

For å lese kommandolinje-argumenter i Bash, kan du bruke den innebygde variabelen `$@` som representerer en liste av alle argumentene som ble sendt til skriptet eller programmet når det ble kjørt. La oss ta en titt på et enkelt eksempel for å demonstrere dette:

```Bash
#!/bin/bash

echo "Hei, $1. Du er $2 år gammel."
```

Kjører dette skriptet med kommandolinje-argumenter `Maria 30` vil gi følgende output:
```
Hei, Maria. Du er 30 år gammel.
```

Her brukes `$1` og `$2` for å referere til de første og andre argumentene som er sendt til skriptet. Du kan også bruke `shift` kommandoen for å flytte fra ett argument til det neste i listen, hvis du trenger å behandle flere argumenter i skriptet ditt.

## Dypdykk

Nå som du vet hvordan du kan lese kommandolinje-argumenter i Bash, kan det være nyttig å forstå mer om hvordan argumentene blir behandlet og tolket av operativsystemet. Argumentene blir lagret i en array-kommando når de blir sendt til skriptet ditt, og du kan bruke `declare` kommandoen for å se på denne arrayen. For eksempel:

```Bash
#!/bin/bash

declare -p@
```

Dette vil skrive ut en liste over alle argumentene som er sendt til skriptet ditt, noe som kan være nyttig for debugging formateringen av argumentene i koden din.

## Se også

For mer informasjon om Bash-programmering og kommandolinje-argumenter, sjekk ut disse nyttige ressursene:

- [Bash Guide for nybegynnere](http://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [GNU Bash Referansehåndbok](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [En introduksjon til Bash arrays](https://www.gnu.org/software/bash/manual/html_node/Arrays.html)
- [Kommandoer for terminalemulering i Bash](https://bash.cyberciti.biz/guide/Reads_from_the_shell_arguments)
- [En praktisk innføring i Bash skripting](https://ryanstutorials.net/bash-scripting-tutorial/bash-globals.php)