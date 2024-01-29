---
title:                "Samenvoegen van strings"
date:                  2024-01-28T21:56:37.247819-07:00
model:                 gpt-4-0125-preview
simple_title:         "Samenvoegen van strings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Strings aan elkaar koppelen betekent dat je ze aan elkaar plakt om een nieuwe string te vormen. Programmeurs doen dit om tekst op dynamische wijzen te combineren, zoals het samenstellen van berichten of het genereren van bestandspaden.

## Hoe:

In C gebruik je de `strcat` functie uit `string.h` om strings aan elkaar te koppelen. Maar let op, je hebt een bestemmingsarray nodig die groot genoeg is om het gecombineerde resultaat te bevatten.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char bestemming[50] = "Hallo, ";
    char bron[] = "wereld!";

    // Koppel `bron` aan `bestemming`
    strcat(bestemming, bron);

    // Geef de gekoppelde string weer
    printf("%s\n", bestemming); // "Hallo, wereld!"

    return 0;
}
```

Zorg ervoor dat je bestemmingsarray niet overloopt. Dat is jouw taak, want C doet dat niet voor je.

## Dieper Duiken

Aaneenschakeling is een basis tekstbewerking sinds de vroege computertechniek. In C doen functies zoals `strcat` en `strncat` (die het aantal aaneengeschakelde karakters beperkt) het zware werk. C beheert het geheugen niet voor je, dus onthoud om genoeg ruimte toe te wijzen voordat je gaat koppelen.

Alternatieven? Oh, zeker. Als je je zorgen maakt over bufferoverflows, kun je `snprintf` gebruiken. Het is veiliger omdat het je toestaat om de maximale grootte van de outputbuffer te specificeren:

```C
char buffer[50];
snprintf(buffer, 50, "%s%s", "Hallo, ", "wereld!");
```

Wat betreft de details, `strcat` werkt door het einde van de eerste string te vinden en daar karakter voor karakter de tweede string te kopiëren. Eenvoudig, maar het handmatige geheugenbeheer maakt het gevoelig voor fouten zoals bufferoverlopen.

## Zie Ook

- C Standard Library documentatie voor `strcat`: https://en.cppreference.com/w/c/string/byte/strcat
- Veilig coderen in C: https://wiki.sei.cmu.edu/confluence/display/c/SEI+CERT+C+Coding+Standard
- Leer meer over bufferoverlopen: https://owasp.org/www-community/attacks/Buffer_overflow
