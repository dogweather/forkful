---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "Hur man: Jag g\xF6r detta tillr\xE4ckligt ofta f\xF6r att jag ska ha\
  \ omstrukturerat det till denna enkla `delete()` funktion. Det \xE4r ocks\xE5 en\
  \ bra demonstration av\u2026"
lastmod: '2024-04-05T21:53:38.794319-06:00'
model: gpt-4-0125-preview
summary: "Jag g\xF6r detta tillr\xE4ckligt ofta f\xF6r att jag ska ha omstrukturerat\
  \ det till denna enkla `delete()` funktion."
title: "Radera tecken som matchar ett m\xF6nster"
weight: 5
---

## Hur man:
```Python
import re

# Exempelsträng
text = "Hej, världen! 1234"

# Ta bort alla siffror
inga_siffror = re.sub(r'\d', '', text)
print(inga_siffror)  # Utdata: "Hej, världen! "

# Ta bort skiljetecken
inga_skiljetecken = re.sub(r'[^\w\s]', '', text)
print(inga_skiljetecken)  # Utdata: "Hej världen 1234"

# Ta bort vokaler
inga_vokaler = re.sub(r'[aeiouAEIOU]', '', text)
print(inga_vokaler)  # Utdata: "Hj, vrldn! 1234"
```

### Min anpassade funktion

Jag gör detta tillräckligt ofta för att jag ska ha omstrukturerat det till denna enkla `delete()` funktion. Det är också en bra demonstration av [doctests](https://docs.python.org/3/library/doctest.html):

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hej, världen!", "l")
    'Hej, väden!'

    >>> delete("Hej, världen!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", string)
```

## Fördjupning
Praktiken att ta bort tecken som matchar ett mönster i text har djupa rötter inom datavetenskapen, spårandes tillbaka till tidiga Unix-verktyg som `sed` och `grep`. I Python tillhandahåller `re`-modulen denna förmåga, genom att använda reguljära uttryck—ett kraftfullt och mångsidigt verktyg för textbearbetning.

Alternativ till `re`-modulen inkluderar:
- Strängmetoder som `replace()` för enkla fall.
- Tredjepartsbibliotek som `regex` för mer komplexa mönster och bättre Unicode-stöd.

Bakom kulisserna, när du använder `re.sub()`, kompilerar Python-tolken mönstret till en serie bytekoder, behandlade av en tillståndsmaskin som utför mönstermatchning direkt på inmatningstexten. Denna operation kan vara resurskrävande för stora strängar eller komplexa mönster, så prestandaöverväganden är avgörande för bearbetning av stora data.

## Se även
- [Python `re`-modulens dokumentation](https://docs.python.org/3/library/re.html): Officiella dokument för reguljära uttryck i Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): En omfattande guide till reguljära uttryck.
- [Real Python-tutorial om regex](https://realpython.com/regex-python/): Verkliga tillämpningar av reguljära uttryck i Python.
