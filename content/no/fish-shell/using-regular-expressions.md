---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Regulære uttrykk er en teknikk for å søke og erstatte tekst ved å bruke et mønster. Programmerere bruker dem fordi de er kraftig og fleksibel for teksthåndtering.

## Hvordan:

```Fish Shell
# Eksempel: Finne alle filer med 'log' i navnet
ls | grep 'log'

# Utfall:
error_log
access_log

# Eksempel: Erstatte 'foo' med 'bar' på tvers av flere filer
sed -i 's/foo/bar/g' *.txt
```

## Dypdykk

Regulære uttrykk har eksistert siden 1950-årene, utviklet fra automatteorien. Alternativer inkluderer strengmatching og innebygde funksjoner i programmeringsspråk, men regulære uttrykk tilbyr mer kompleksitetskontroll. For implementasjon i Fish Shell, `grep` og `sed` er vanlige verktøy som benytter PCRE (Perl Compatible Regular Expressions).

## Se Også

- Official Fish documentation: https://fishshell.com/docs/current/index.html
- Grep manual page: https://man7.org/linux/man-pages/man1/grep.1.html
- Sed manual page: https://man7.org/linux/man-pages/man1/sed.1.html
- Regular expressions in-depth tutorial: https://www.regular-expressions.info/tutorial.html
