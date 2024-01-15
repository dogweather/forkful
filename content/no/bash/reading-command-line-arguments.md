---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "Bash: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Lesing av kommandolinjeargumenter kan virke som noe som bare proffe datanerder trenger å forstå, men det kan faktisk være nyttig for alle som bruker Bash-kommandolinjen ofte. Ved å lese og håndtere argumenter på riktig måte, kan du gjøre kommandolinjen mer effektiv og spare tid på å gjenta kommandoer.

## Hvordan gjøre det
For å lese kommandolinjeargumenter i Bash, bruker du variablene "$1", "$2", osv. Disse representerer de ulike argumentene som blir gitt til kommandoen din når du kjører den. La oss si at du vil lage en enkel skript som tar to tall som argumenter og skriver ut summen av de to tallene. Du kan gjøre det på følgende måte:

```Bash
#!/bin/bash
sum=$(($1 + $2))
echo "Summen av $1 og $2 er $sum"
```

Her användes "$1" og "$2" i variabelen "sum" for å legge sammen tallene som ble gitt som argumenter. Skriptet blir deretter kjørt ved å skrive "bash script.sh 5 7" i kommandolinjen, noe som vil gi følgende output:

```Bash
Summen av 5 og 7 er 12
```

Dette er en enkel måte å lese og bruke kommandolinjeargumenter på i Bash, og kan være nyttig for å automatisere enkle oppgaver.

## Dypdykk
Hvis du ønsker å gå enda dypere inn i hvordan kommandolinjeargumenter fungerer i Bash, er det lurt å lese på dokumentasjonen til Bash. Det finnes også en rekke gode ressurser på nett som kan hjelpe deg med å forstå ulike aspekter av dette, som f.eks. [denne artikkelen](https://www.baeldung.com/linux/bash-command-line-arguments) som tar for seg flere forskjellige måter å håndtere kommandolinjeargumenter på. Det kan også være lurt å eksperimentere med forskjellige kommandoer og se hvordan de påvirker variablene "$1", "$2", osv.

## Se også
- [Dokumentasjonen til Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [En grundig guide til kommandolinjeargumenter i Bash](https://www.baeldung.com/linux/bash-command-line-arguments)
- [En liste over ulike tips og triks for kommandolinjen i Bash](https://zhoutall.com/my-cmd-for-bash-confirm/)