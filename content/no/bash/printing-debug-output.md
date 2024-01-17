---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Bash: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Printing debug output, eller utskrift av feilsøkingsinformasjon, er en essensiell del av programmering. Det refererer til å skrive ut informasjon om hva som skjer i programmet under kjøringen, som kan hjelpe utvikleren med å identifisere og fikse eventuelle feil. Dette gjøres vanligvis ved å bruke spesielle kommandoer for å sende ut spesifikke beskjeder til konsollen eller en fil.

## Slik gjør du det:

For å skrive ut enkel tekst, kan du bruke kommandoen `echo` etterfulgt av teksten du ønsker å skrive ut mellom doble anførselstegn. For eksempel: 

```Bash
echo "Dette er en debug-melding"
```

Du kan også bruke `printf` kommandoen for å formatere utskriften mer nøyaktig. For eksempel:

```Bash
printf "Feilen skjedde på linje %d" "$LINENO"
```

## Dypdykk:

Printing debug output har vært en de facto standard i programmering siden de tidligste dagene av Unix og Linux-operativsystemer. Det er en rask og enkel måte å feilsøke på og kan hjelpe til med å finne årsaken til komplekse feil. Alternativer til å skrive ut feilsøkingsinformasjon inkluderer å bruke en debugger, som kan være mer effektivt, men også mer komplekst. Implementeringen av printing debug output varierer avhengig av programmeringsspråk, men de fleste språk har spesifikke kommandoer for å skrive ut informasjon til konsollen eller en fil.

## Se også:

- [Unix Toolbox: Debugging](http://cb.vu/unixtoolbox.xhtml#debugging)
- [The Art of Debugging](https://www.the-art-of-debugging.com/why-do-we-need-debugging/)