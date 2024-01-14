---
title:    "Python: Skriving til standard-feil"
keywords: ["Python"]
---

{{< edit_this_page >}}

#.
## Hvorfor

Det å skrive til standard error i Python kan være en nyttig måte å håndtere og fange feil og unntak i koden din. Det kan også hjelpe med å feilsøke og forbedre ytelsen til programmet ditt.

## Hvordan

For å skrive til standard error i Python, bruker du funksjonen `sys.stderr.write()` og passerer inn teksten du vil skrive som en streng. Dette vil skrive teksten til standard error-strømmen i stedet for standard output. Se et eksempel nedenfor:

```Python
import sys
sys.stderr.write("Det er en feil i koden din!")
```

Output:
```Python
Det er en feil i koden din!
```

## Dypdykk

Det er viktig å merke seg at standard error-strømmen har en høyere prioritet enn standard output-strømmen. Det betyr at hvis det oppstår en feil i koden din og du ikke har håndtert den, vil den bli skrevet ut til standard error i stedet for standard output. Dette er nyttig når du bruker programmet ditt interaktivt, da standard output kan brukes til å vise ønsket informasjon, mens feilmeldinger skrives til standard error.

En annen måte å skrive til standard error i Python er å bruke modulen `logging`. Dette gir deg mer kontroll over hvordan feilmeldinger håndteres og skrives til forskjellige logger. Se dokumentasjonen for mer informasjon om denne modulen.

## Se også

- [Python dokumentasjon for sys modulet](https://docs.python.org/3/library/sys.html)
- [Python dokumentasjon for logging modulet](https://docs.python.org/3/library/logging.html)