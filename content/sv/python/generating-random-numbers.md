---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga nummer innebär att skapa nummer som inte är förutsägbara och inte heller följer någon känd mönster. Programmerare gör det för att lägga till slumpmässighet i deras program, vilket är användbart för allt från spel till säkerhetsprotokoll.

## Så här gör du:
Python erbjuder `random` modulen för att generera slumpmässiga nummer. Se nedanstående exempel:

```Python
import random

print(random.randint(1, 100))  # Skriver ut ett slumpmässigt heltal mellan 1 och 100.
```
När du kör koden ovan kan utfallet ändras varje gång, vilket är tecknet på ett slumpmässigt nummer.

## Djupdykning
iPython:s `random` modul lanserades i mitten på 90-talet. Innan dess var de vanligaste sätten att generera slumpmässiga nummer att använda tidsstämplar eller hårdvaruinmatningar.

Alternativt till `random` finns andra Python-moduler som `numpy.random` eller `secrets`, beroende på dina behov. Exempelvis` secrets` är mer inriktat på kryptografisk säkerhet.

När det gäller att generera slumpmässiga nummer, är det viktigt att notera entropi och seeds. "Seeds” används för att inleda en rad av slumpmässiga nummer och entropi hänvisar till osäkerhetsmängden eller slumpmässigheten.

## Se också
För mer information om Python:s `random` modul, se [Random - Generating random numbers](https://docs.python.org/3/library/random.html). För att lära dig mer om alternativ till `random` modulen, se [numpy.random](https://numpy.org/doc/stable/reference/random/index.html) och [secrets](https://docs.python.org/3/library/secrets.html).