---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:14:22.866990-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
"Hva & Hvorfor?" - Å hente dagens dato i programmering betyr å lese det nøyaktige dato og klokkeslett fra systemet. Vi gjør dette for å legge til tidsstempler, håndtere tidsfrister eller for datodrevne logikk.

## How to:
"Slik gjør du det:" - I Fish Shell henter du enkelt dato med innebygde funksjoner.

For å få dagens dato i formatet ÅÅÅÅ-MM-DD:
```Fish Shell
set -l today (date +%Y-%m-%d)
echo $today
```
Sample output:
```
2023-04-02
```

For å inkludere klokkeslett:
```Fish Shell
set -l now (date +%Y-%m-%d\ %H:%M:%S)
echo $now
```
Sample output:
```
2023-04-02 15:45:10
```

## Deep Dive
"Dypdykk:" - Før Fish Shell eksisterte, brukte mange Bash. Overgangen til Fish introduserte en mer moderne og brukervennlig opplevelse med fokus på interaktivitet og smidighet. I begge skallene er `date`-kommandoen sentral for tidshåndtering, men Fishens skriptsyntaks skiller seg ut for sin rene og leslige natur.

Videre har alternativer som Python, JavaScript, og PHP sine egne innebygde måter å hente dato på, men `date`-kommandoen i skallet ditt er ofte den raskeste veien til mål for enkle oppgaver.

Når det kommer til implementasjon, bruker Fish `date`-kommandoen som kjører i underliggende operativsystemet. Outputformatet bestemmes ved å sette argumenter som `%Y` for år, `%m` for måned, og `%d` for dag, som åpner for fleksibel datoformatting.

## See Also
"Se Også:" - For mer utfyllende info om Fish Shell:

- Fish Shell GitHub-repositoriet: [github.com/fish-shell/fish-shell](https://github.com/fish-shell/fish-shell)
- En guide til dato og klokkeslett i Unix/Linux: [man7.org/linux/man-pages/man1/date.1.html](https://man7.org/linux/man-pages/man1/date.1.html)
