---
date: 2024-01-20 17:35:51.419531-07:00
description: "How to: Konvertering av dato til tekst bruker ofte `date`-kommandoen\
  \ i Bash, som har r\xF8tter tilbake til de f\xF8rste versjonene av UNIX. Du kan\
  \ bruke The\u2026"
lastmod: '2024-04-05T21:53:41.947790-06:00'
model: gpt-4-1106-preview
summary: "Konvertering av dato til tekst bruker ofte `date`-kommandoen i Bash, som\
  \ har r\xF8tter tilbake til de f\xF8rste versjonene av UNIX."
title: Konvertere en dato til en streng
weight: 28
---

## How to:
```Bash
# For å vise dagens dato:
echo "Dagens dato er: $(date '+%Y-%m-%d')" # f.eks. 2023-04-05

# For å skrive ut en dato med norsk format:
echo "Datoen i norsk format er: $(LANG=no_NO date '+%d.%m.%Y')" # f.eks. 05.04.2023

# For å konvertere en spesifikk dato:
dato="2023-04-05"
dato_str=$(date -d $dato '+%A, %d %B %Y')
echo "Konvertert datostr: $dato_str" # f.eks. Onsdag, 05 april 2023
```

## Deep Dive
Konvertering av dato til tekst bruker ofte `date`-kommandoen i Bash, som har røtter tilbake til de første versjonene av UNIX. Du kan bruke The `date`-kommandoen for å formatere tid og dato, basert på forskjellige parametre. Som alternativer til `date` kan du bruke programmeringsspråk som Python eller Perl, som gir større fleksibilitet og funksjonalitet.

Detaljer om implementasjon av `date` inkluderer bruk av formatstrenger som `%Y` for år, `%m` for måned og `%d` for dag. Du kan også sette tidszoner og håndtere lokalisering, som for eksempel ved å bruke `LANG=no_NO` for å få norsk dato-format.

## See Also
- `man date`: https://linux.die.net/man/1/date
- Bash Date/Time Functions: https://www.gnu.org/software/coreutils/manual/html_node/Date-invocation.html
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/dates.html
