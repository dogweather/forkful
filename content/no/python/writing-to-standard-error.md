---
title:    "Python: Skriver til standardfeil"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor ville noen engasjere seg i å skrive til standard error i Python? Det kan være nyttig når man ønsker å skrive ut feilmeldinger eller annen informasjon som ikke skal blandes sammen med utgangen til programmet. Det kan også være nyttig når man vil debugge og finne eventuelle feil i koden.

# Hvordan

Det er enkelt å skrive til standard error i Python ved hjelp av en innebygd funksjon kalt "print" med et ekstra argument "file=sys.stderr". Dette vil sende teksten til standard error i stedet for standard utgang.

```Python
import sys
print("Dette vil bli skrevet til standard error", file=sys.stderr)
```

Dette vil gi følgende output:

```
Dette vil bli skrevet til standard error
```

Her blir teksten skrevet ut i rødt, noe som tydelig markerer at det kommer fra standard error og ikke standard utgang.

# Dypdykk

Å forstå forskjellen mellom standard error og standard utgang er viktig når man jobber med Python. Standard utgang er en stream der programmet skriver ut resultatet sitt, mens standard error er en stream der det skrives ut feilmeldinger og annen informasjon som ikke skal blandes sammen med resultatet. Det kan være lurt å alltid skrive feilmeldinger til standard error, slik at det blir enklere å finne og rette eventuelle feil i koden.

# Se også

Her er noen relevante lenker for videre lesning:

- [Dokumentasjon for standard bibliotek i Python](https://docs.python.org/3/library/sys.html#sys.stderr)
- [Artikkel om å skrive til standard error i Python](https://www.geeksforgeeks.org/python-print-on-sys-stderr/)
- [Video tutorial om å skrive til standard error i Python](https://www.youtube.com/watch?v=3LmAy6L-unE)