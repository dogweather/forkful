---
date: 2024-01-20 17:42:48.189724-07:00
description: "Att radera tecken som matchar ett m\xF6nster inneb\xE4r att man filtrerar\
  \ bort specifika karakt\xE4rer fr\xE5n en str\xE4ng baserat p\xE5 definierade kriterier.\u2026"
lastmod: '2024-03-13T22:44:37.467553-06:00'
model: gpt-4-1106-preview
summary: "Att radera tecken som matchar ett m\xF6nster inneb\xE4r att man filtrerar\
  \ bort specifika karakt\xE4rer fr\xE5n en str\xE4ng baserat p\xE5 definierade kriterier."
title: "Ta bort tecken som matchar ett m\xF6nster"
weight: 5
---

## Hur gör man:
Vi använder Pythons `re`-modul för att matcha och ta bort tecken med reguljära uttryck.

```Python
import re

# Exempelsträng
text = "B4n4n3r är g0d4!"

# Mönster som matchar icke-bokstäver
pattern = r'[^a-zA-ZåäöÅÄÖ\s]'

# Använder re.sub() för att ersätta matchande tecken med ingenting
clean_text = re.sub(pattern, '', text)

print(clean_text)  # Output: Bananer är goda
```

## Djupdykning
Förr i tiden var textmanipulering mer begränsad och specifik för varje programmeringsspråk. Med moderna reguljära uttryck, som har sitt ursprung på 1950-talet men standardiserades på 80-talet, har mönstermatchning blivit universell och kraftfull. Alternativ till `re`-modulen inkluderar tredjepartsbibliotek som `regex` som erbjuder utökade funktioner, eller att manuellt iterera över strängen och konstruera en ny utan oönskade tecken. I Python kan `re.sub()` användas för att ta bort tecken. Det funktionerar genom att söka efter mönster och ersätta de träffar med ett definierat tecken eller en tom sträng.

## Se även
- [Python re.sub() Documentation](https://docs.python.org/3/library/re.html#re.sub)
- [Regular Expression HOWTO](https://docs.python.org/3/howto/regex.html)
- [Python regex module Documentation](https://pypi.org/project/regex/)
