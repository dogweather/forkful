---
title:                "Bruk av regulære uttrykk"
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regulære uttrykk er mønstersøk i tekst. Programmerere bruker det til å finne, erstatte og manipulere tekst raskt og nøyaktig.

## How to:
```Python
import re

# Finn alle forekomster av 'hund'
tekst = "Katten lekte med hunden. En annen hund sov."
mønster = r"hund"
funn = re.findall(mønster, tekst)
print(funn)  # Output: ['hund', 'hund']

# Bytt ut 'katt' med 'mus'
ny_tekst = re.sub(r"katt", "mus", tekst)
print(ny_tekst)  # Output: "Musen lekte med hunden. En annen hund sov."
```

## Deep Dive
Regulære uttrykk har røtter i teoretisk informatikk og formalisert i 1950-årene. Alternativer inkluderer biblioteker som `string` for basis tekstbehandling. Implementasjonsdetaljer i Python håndteres av `re`-modulen, som støtter en rekke operasjoner for kompleks tekstmanipulasjon.

## See Also
- Python `re` moduldokumentasjon: https://docs.python.org/3/library/re.html
- RegexOne for å lære regulære uttrykk: https://regexone.com/
- Regulære uttrykk 101, for å teste uttrykk online: https://regex101.com/