---
title:                "Beregning av datoer i fremtiden eller fortiden"
html_title:           "Bash: Beregning av datoer i fremtiden eller fortiden"
simple_title:         "Beregning av datoer i fremtiden eller fortiden"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden er prosessen med å finne en bestemt dato basert på et gitt antall dager fra en annen dato. For eksempel, å beregne datoen som er 30 dager fra i dag. Programmere må gjøre dette for å håndtere ulike datoer og kalkulere fremtidige eller fortidsdatoer.

# Hvordan:
Bash har innebygde funksjoner for å beregne en dato i fremtiden eller fortiden. Du kan bruke disse funksjonene ved å legge til eller trekke fra et spesifisert antall dager fra en gitt dato.

```bash
# Beregne datoen som er 30 dager fra i dag
date -d "+30 days"
```

Resultat: Sat Oct 16 00:00:00 CEST 2021

```bash
# Beregne datoen som er 10 dager før i dag
date -d "-10 days"
```

Resultat: Sun Sep 26 00:00:00 CEST 2021

# Dypdykk:
I følge Unix-tidsstempelet, 1. januar 1970 kl. 00:00:00 er referansedatoen for å beregne datoer i fremtiden eller fortiden. Alternativt kan du bruke tredjepartsbiblioteker som moment.js for mer komplekse datooperasjoner. Implementeringsdetaljer varierer avhengig av språk og plattform, men konseptet er det samme.

# Se også:
- [Bash's date command documentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Moment.js](https://momentjs.com/) for mer komplekse datooperasjoner