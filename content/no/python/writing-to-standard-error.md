---
title:                "Python: Skriving til standardfeil"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hvorfor skrive til standard error i Python?

Hvorfor bry seg med å skrive til standard error? Svaret er enkelt - å skrive til standard error gir deg muligheten til å skrive ut feilmeldinger og annen informasjon som er viktig for å feilsøke og forbedre din kode.

## Hvordan gjøre det

For å skrive til standard error i Python, bruker du den innebygde funksjonen `sys.stderr.write()`. Du må også importere `sys` biblioteket som gir deg tilgang til standard error kanalen.

```Python
import sys
```

Du kan da bruke `sys.stderr.write()` til å skrive ut meldinger til standard error. For eksempel:

```Python
sys.stderr.write("Dette er en feilmelding\n")
```

Dette vil skrive ut en feilmelding til konsollen, og vil se slik ut:

```
Dette er en feilmelding
```

## Dypdykk

Nå som du vet hvordan du kan skrive til standard error, la oss ta en dypere titt på hvorfor dette kan være nyttig. Når du skriver ut feilmeldinger til standard error i stedet for standard out, kan du enkelt skille ut disse meldingene og behandle dem separat. Dette kan være svært nyttig når du arbeider med større programmer eller prosjekter, da det gjør det enklere å finne og fikse feil.

En annen fordel er at du kan skrive til både standard error og standard out samtidig, og dermed få både feilmeldinger og andre utskrifter i samme konsoll. Dette kan gjøre feilsøkingen mye enklere.

En ting å merke seg er at når du skriver til standard error, vil denne utskriften ikke bli omadressert hvis du bruker `>` eller `|` i terminalen. Dette betyr at standard error vil bli vist på skjermen, selv om du omdirigerer standard out.

# Se også

- [Python dokumentasjon: sys.stderr](https://docs.python.org/3/library/sys.html#sys.stderr)
- [Programiz: Standard Input, Output, and Error Streams in Python](https://www.programiz.com/python-programming/io)
- [Real Python: Python's print() Function](https://realpython.com/python-print/)