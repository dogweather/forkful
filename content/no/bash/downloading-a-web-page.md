---
title:                "Nedlasting av en nettside"
html_title:           "Bash: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Å laste ned en nettside med Bash kan være nyttig for å automatisere oppgaver eller for å få tilgang til informasjon som ikke er tilgjengelig gjennom konvensjonelle nettlesere.

## Hvordan

```Bash
#!/bin/bash

# Lag en fil for å lagre nettsiden
touch nettside.html

# Last ned nettsiden og lagre som HTML-fil
curl https://www.example.com > nettside.html

# Vis innholdet av nettsiden
cat nettside.html

# Slette den midlertidige HTML-filen
rm nettside.html
```

**Output:** Nettsiden vil bli lastet ned og vist i terminalen som ren HTML-kode. Filen vil bli lagret i samme mappe som Bash-skriptet, med mindre annet er angitt i `curl`-kommandoen.

## Dypdykk

Bash har en innebygd kommando, `curl`, som er spesialdesignet for å laste ned innhold fra internett. Det finnes også andre verktøy som kan brukes, som for eksempel `wget`.

En annen måte å laste ned en nettside på er å bruke `lynx`-kommandoen, men dette vil vise nettsiden i et tekstbasert grensesnitt i stedet for å laste ned som ren HTML-kode.

Et viktig aspekt ved å laste ned nettsider med Bash er å være respektfull og følge nettstedets retningslinjer og eventuelle begrensninger som er satt opp. For eksempel, ikke last ned store mengder av innhold i en kort periode for å unngå overbelastning av nettstedets servere.

## Se også

[99 Bash-skripteksempler du kan bruke i hverdagen](https://linuxhint.com/99_bash_examples/)

[Bash Guide for Nybegynnere](https://tldp.org/LDP/Bash-Beginners-Guide/html/)

[Bash og nettsider - Hvordan laste ned og behandle HTML-innhold](https://www.howtogeek.com/137295/how-to-download-and-process-html-from-the-web-in-bash/)