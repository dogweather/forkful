---
date: 2024-01-20 17:58:44.695135-07:00
description: "Hur g\xF6r man?: S\xF6k och ers\xE4tt-funktionen \xE4r gammal s\xE5\
  \ in i Norden. T\xE4nk textredigerare fr\xE5n 70-talet typ 'vi' och 'sed'. Python\
  \ implementerar detta genom\u2026"
lastmod: '2024-04-05T22:50:51.749850-06:00'
model: gpt-4-1106-preview
summary: "S\xF6k och ers\xE4tt-funktionen \xE4r gammal s\xE5 in i Norden."
title: "S\xF6kning och ers\xE4ttning av text"
weight: 10
---

## Hur gör man?:
```Python
text = "Hej värld! Programmering är kul. Hej värld!"

# Sök och ersätt
ersatt_text = text.replace("Hej", "Hejdå")

print(ersatt_text)
```
Output:
```
Hejdå värld! Programmering är kul. Hejdå värld!
```

Avancerat exempel med reguljära uttryck:
```Python
import re

text = "Hej värld! 2023 är året då vi kodar i Python."

# Sök och ersätt med reguljära uttryck
ersatt_text = re.sub(r"\d+", "[CENSURERAT]", text)

print(ersatt_text)
```
Output:
```
Hej värld! [CENSURERAT] är året då vi kodar i Python.
```

## Fördjupning:
Sök och ersätt-funktionen är gammal så in i Norden. Tänk textredigerare från 70-talet typ 'vi' och 'sed'. Python implementerar detta genom strängmetoden `replace()` och `re`-modulen för regex (reguljära uttryck). Regex är kraftfullt men kan vara krångligt. Det finns också externa bibliotek som `regex` som erbjuder mer avancerade funktioner än inbyggda `re`.

## Se även:
- Python dokumentation för strängmetoder: https://docs.python.org/3/library/stdtypes.html#string-methods
- Python dokumentation för `re`-modulen: https://docs.python.org/3/library/re.html
- Reguljära uttryck 101, användbar för att testa och lära sig regex: https://regex101.com/
- Artikel om att använda `regex`-biblioteket: https://pypi.org/project/regex/
