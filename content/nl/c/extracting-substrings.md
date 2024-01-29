---
title:                "Substrings extraheren"
date:                  2024-01-28T21:59:32.019073-07:00
model:                 gpt-4-0125-preview
simple_title:         "Substrings extraheren"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het extraheren van deelreeksen betekent het grijpen van een specifiek deel van een reeks - een stuk van de taart. Programmeurs doen dit om alleen de relevante gegevens in een grotere tekst te isoleren, te verwerken of te manipuleren.

## Hoe te:
Laten we enkele deelreeksen uit een reeks plukken met C.

```C
#include <stdio.h>
#include <string.h>

void extract_substring(const char *source, int start, int length, char *dest) {
    strncpy(dest, source + start, length);
    dest[length] = '\0'; // Vergeet niet te null-termineren!
}

int main() {
    const char *full_text = "Extracting substrings is neat.";
    char snippet[20];

    extract_substring(full_text, 0, 10, snippet);
    printf("Fragment: %s\n", snippet);

    extract_substring(full_text, 12, 10, snippet);
    printf("Nog een: %s\n", snippet);

    return 0;
}
```

Voorbeelduitvoer:

```
Fragment: Extracting
Nog een: substrings
```

## Diepe Duik
Het extraheren van deelreeksen is absoluut niet nieuw. In de C-programmeerwereld is het sinds de conceptie van de taal in de jaren 70 een gangbare taak geweest.

Je hebt alternatieve manieren om die deelreeksen te pakken te krijgen. Sommige mensen gebruiken `strncpy()`, zoals ons voorbeeld hierboven. Anderen geven misschien de voorkeur aan `sscanf()` of zelfs handmatig door de reeks heen lopen. Elke aanpak heeft zijn nuances. Let op met `strncpy()`: als de lengte die je opgeeft verder reikt dan het einde van de reeks, zal het niet voor jou null-termineren.

Achter de schermen is een reeks in C gewoon een array van karakters. Bij het snijden heb je te maken met pointers naar specifieke adressen in het geheugen. Let op de grenzen en null-termmineer altijd je fragmenten.

## Zie Ook
- `strncpy()` handleiding: https://www.man7.org/linux/man-pages/man3/strncpy.3.html
- C String behandeling: https://en.cppreference.com/w/c/string
- Pointers en arrays: https://www.tutorialspoint.com/cprogramming/c_pointers.htm
