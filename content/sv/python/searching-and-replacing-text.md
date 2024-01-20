---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Sökning och ersättning av text är en operation där specifika textdelar identifieras och byts ut med annan text. Det är en grundläggande funktion i programmering för att snabbt och effektivt ändra kod, rensa data, och mer.

## Hur man gör:

Enkelt söka och ersätta i Python kan göras med strängmetoden `replace()`. Till exempel:

```Python
text = "Hej världen"
text = text.replace("Hej", "Hallå")
print(text)
```

Utskriften kommer att bli:

```
Hallå världen
```

Om du vill söka och ersätta med användning av reguljära uttryck, kan du använda `re`-modulen.

```Python
import re

text = "Hej världen, Hej igen"
text = re.sub(r'Hej', 'Hallå', text)
print(text)
```

Detta kommer att ge utskriften:

```
Hallå världen, Hallå igen
```

## Djupdykning

Historiskt sett finns sök och ersätt från de första textredigerarna. Sedan dess har det blivit ett grundläggande verktyg i de flest programmeringsspråk.

Det finns även andra sätt att söka och byta ut text i Python, som med listor av strängar, eller med mer komplext strängmanipulering. Men generellt sett är `str.replace()` och `re.sub()` de mest direkta och lättanvända metoderna.

`str.replace()` fungerar genom att scanna igenom strängen från början till slut, söker efter specifika tecken eller sekvenser och byter dessa. `re.sub()` gör samma sak, men med möjlighet att använda reguljära uttryck för att matcha mer komplexa mönster.

## Se även

Det finns många ytterligare resurser för att lära sig mer om att söka och ersätta text i Python, inklusive:
- [Python's official documentation for str.replace()](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Python's official documentation for re.sub()](https://docs.python.org/3/library/re.html#re.sub)
- [Intro till reguljära uttryck i Python](https://realpython.com/regex-python/)