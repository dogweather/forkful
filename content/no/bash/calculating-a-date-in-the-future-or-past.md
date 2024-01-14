---
title:    "Bash: Beregning av en dato i fremtiden eller fortiden."
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor

Å beregne datoer i fremtiden eller fortiden kan være nyttig for å lage dynamiske skript eller for å holde oversikt over datoer for viktige hendelser. Det kan også være nyttig for å planlegge fremtidige avtaler eller for å beregne aldre.

## Hvordan gjøre det

Det finnes flere måter å beregne datoer i fremtiden eller fortiden ved hjelp av Bash-programmering. Her er noen eksempler:

```Bash
# Beregne dato for 10 dager siden
date -d "10 days ago"
```

```Bash
# Beregne dato for 2 uker frem i tid
date -d "2 weeks"
```

```Bash
# Beregne dato for 6 måneder siden og formatere resultatet
date -d "6 months ago" +"%d %B %Y"
```

## Dypdykk

Bash har mange nyttige kommandoer for å beregne datoer, inkludert `date`, `cal`, `time` og `sleep`. Disse kan kombineres og brukes til å løse mer komplekse utfordringer, som å beregne antall dager mellom to datoer eller å lage en dynamisk kalender.

En annen nyttig funksjon er å bruke variabler til å holde styr på datoer og bruke matematiske operasjoner for å endre datoer. For eksempel kan man legge til eller trekke fra et visst antall dager fra en variabel som inneholder dagens dato.

## Se også

- [Bash Programming](https://www.makeuseof.com/tag/write-simple-bash-script/)
- [Linux Command Line](https://linuxize.com/post/bash-math-calculation/)
- [GNU Coreutils](https://www.gnu.org/software/coreutils/manual/html_node/Examples-of-date.html#Examples-of-date)