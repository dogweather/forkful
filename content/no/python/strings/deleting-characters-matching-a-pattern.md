---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "Slik gj\xF8r du det: ."
lastmod: '2024-04-04T02:02:34.738312-06:00'
model: gpt-4-0125-preview
summary: .
title: "Slette tegn som samsvarer med et m\xF8nster"
weight: 5
---

## Slik gjør du det:
```Python
import re

# Eksempelstreng
tekst = "Hallo, Verden! 1234"

# Fjern alle sifre
ingen_sifre = re.sub(r'\d', '', tekst)
print(ingen_sifre)  # Utdata: "Hallo, Verden! "

# Fjern tegnsetting
ingen_tegnsetting = re.sub(r'[^\w\s]', '', tekst)
print(ingen_tegnsetting)  # Utdata: "Hallo Verden 1234"

# Fjern vokaler
ingen_vokaler = re.sub(r'[aeiouAEIOU]', '', tekst)
print(ingen_vokaler)  # Utdata: "Hll, Vrdn! 1234"
```

### Min egendefinerte funksjon

Jeg gjør dette ofte nok til at jeg omstrukturerte det til denne enkle `delete()`-funksjonen. Det er også en god demonstrasjon av [doctests](https://docs.python.org/3/library/doctest.html):

```python
def delete(streng: str, regex: str) -> str:
    """
    >>> delete("Hallo, verden!", "l")
    'Hao, verden!'

    >>> delete("Hallo, verden!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", streng)
```



## Dypdykk
Praksisen med å slette tegn som samsvarer med et mønster i tekst har dype røtter i datavitenskap, som går tilbake til tidlige Unix-verktøy som `sed` og `grep`. I Python gir `re`-modulen denne muligheten, og bruker regulære uttrykk – et kraftig og allsidig verktøy for tekstbehandling.

Alternativer til `re`-modulen inkluderer:
- Strengmetoder som `replace()` for enkle tilfeller.
- Tredjepartsbiblioteker som `regex` for mer komplekse mønstre og bedre støtte for Unicode.

Under overflaten, når du bruker `re.sub()`, kompilerer Python-tolken mønsteret til en serie bytecode, behandlet av en tilstandsmaskin som utfører mønstersøking direkte på inngangsteksten. Denne operasjonen kan være ressurskrevende for store strenger eller komplekse mønstre, så ytelseshensyn er avgjørende for behandling av store datamengder.

## Se også
- [Python `re`-modulens dokumentasjon](https://docs.python.org/3/library/re.html): Offisielle dokumenter for regulære uttrykk i Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): En omfattende guide til regulære uttrykk.
- [Real Python-opplæring på regex](https://realpython.com/regex-python/): Praktisk bruk av regulære uttrykk i Python.
