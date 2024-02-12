---
title:                "Ta bort tecken som matchar ett mönster"
aliases:
- sv/python/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:48.189724-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ta bort tecken som matchar ett mönster"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att radera tecken som matchar ett mönster innebär att man filtrerar bort specifika karaktärer från en sträng baserat på definierade kriterier. Programmerare gör detta för att rensa data, validera input eller förbereda text för bearbetning.

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
