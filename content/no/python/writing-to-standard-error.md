---
title:                "Skriver til standardfeil"
html_title:           "Python: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Når vi skriver Python-kode, er det vanlig å bruke "print" -funksjonen for å vise utdata på skjermen. Men noen ganger ønsker vi å vise en feilmelding eller annen informasjon som ikke er en del av det ordinære utskriftet. For dette formålet bruker vi "sys.stderr" -enheten til å skrive til standard error. Dette er spesielt nyttig når vi feilsøker koden vår og ønsker å kommunisere med brukeren på en mer eksplisitt måte.

# Hvordan:

```Python
import sys

print("Dette blir vist på skjermen")
print("Dette blir skrevet til standard error", file=sys.stderr)
```

Output:
Dette blir vist på skjermen
Dette blir skrevet til standard error 

# Dykk dypere:

Å skrive til standard error er ikke en ny teknikk, det har vært en del av programmering i lang tid. Tidligere var dette en av få måter å gi feilmeldinger til brukere på. I Python kan vi også bruke "sys.stdout" -enheten for å skrive utdata på en liknende måte, men da vil det blandes med annet utskrift. En annen mulighet er å bruke "logging" -modulen som gir mer avanserte muligheter for å håndtere utdata og feilmeldinger.

# Se også:

- Python dokumentasjon: https://docs.python.org/3/library/sys.html#sys.stderr
- Logging modulen: https://docs.python.org/3/library/logging.html