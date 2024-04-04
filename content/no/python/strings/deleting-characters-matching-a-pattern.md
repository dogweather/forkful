---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: 'Hvordan: .'
lastmod: '2024-04-04T01:28:33.998193-06:00'
model: gpt-4-0125-preview
summary: .
title: "Slette tegn som matcher et m\xF8nster"
weight: 5
---

## Hvordan:
```Python
import re

# Eksempelstreng
tekst = "Hello, World! 1234"

# Fjern alle tall
ingen_tall = re.sub(r'\d', '', tekst)
print(ingen_tall)  # Utdata: "Hello, World! "

# Fjern tegnsetting
ingen_tegnsetting = re.sub(r'[^\w\s]', '', tekst)
print(ingen_tegnsetting)  # Utdata: "Hello World 1234"

# Fjern vokaler
ingen_vokaler = re.sub(r'[aeiouAEIOU]', '', tekst)
print(ingen_vokaler)  # Utdata: "Hll, Wrld! 1234"
```

### En tilpasset funksjon jeg skrev

Jeg gjør dette ofte nok til at jeg refaktorerte det til denne `slett()`-funksjonen. Det er også en god demonstrasjon av [doctests](https://docs.python.org/3/library/doctest.html):

```python
def slett(streng: str, regex: str) -> str:
    """
    >>> slett("Hello, world!", "l")
    'Heo, word!'

    >>> slett("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", streng)
```


## Dypdykk
Praksisen med å slette tegn som samsvarer med et mønster i tekst har dype røtter i datavitenskap, som går tilbake til tidlige Unix-verktøy som `sed` og `grep`. I Python gir `re`-modulen denne evnen, og benytter regulære uttrykk—et kraftfullt og fleksibelt verktøy for tekstbehandling.

Alternativer til `re`-modulen inkluderer:
- Strengmetoder som `replace()` for enkle tilfeller.
- Tredjepartsbiblioteker som `regex` for mer komplekse mønstre og bedre Unicode-støtte.

I bunn og grunn, når du bruker `re.sub()`, kompilerer Python-tolken mønsteret til en serie med bytekoder, behandlet av en tilstandsmaskin som utfører mønstersammenligning direkte på inngangsteksten. Denne operasjonen kan være ressurskrevende for store strenger eller komplekse mønstre, så ytelseshensyn er avgjørende for behandling av store datamengder.

## Se også
- [Python `re`-modul dokumentasjon](https://docs.python.org/3/library/re.html): Offisielle dokumenter for regulære uttrykk i Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): En omfattende veiledning til regulære uttrykk.
- [Real Python-tutorial om regex](https://realpython.com/regex-python/): Virkelige anvendelser av regulære uttrykk i Python.
