---
title:                "Een string omzetten naar kleine letters"
date:                  2024-01-28T21:57:44.811590-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string omzetten naar kleine letters"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tekenreeks converteren naar kleine letters betekent het wijzigen van alle hoofdletters naar hun kleine letter-tegenhangers. Dit wordt gedaan voor consistentie, zoeken, vergelijken en sorteren, waarbij hoofdlettergevoeligheid roet in het eten kan gooien.

## Hoe te:

In C zou je over het algemeen door de tekenreeks lopen en elk teken converteren. Hier is een snel voorbeeld:

```c
#include <stdio.h>
#include <ctype.h>

void toLowercase(char *str) {
    if (!str) return; // Veiligheidscontrole
    while (*str) {
        *str = tolower((unsigned char)*str); // Converteer elk teken naar kleine letter
        str++; // Ga naar het volgende teken
    }
}

int main() {
    char myStr[] = "Hello, World!";
    toLowercase(myStr);
    printf("%s\n", myStr); // Levert op: hello, world!
    return 0;
}
```

## Diepere Duik

Lang geleden, toen computergeheugens klein waren, maakten mensen zich zorgen om elke byte. Het converteren van tekenreeksen was niet triviaal; het was ruimtebesparend om standaard naar één geval te gaan. Nu gaat het minder om ruimte, meer om functionaliteit.

Waarom `tolower` gebruiken en niet zelf iets ontwikkelen? De C standaardbibliotheek heeft het. Het behandelt eigenaardigheden over verschillende tekensets en locales. Zelf iets ontwikkelen? Je zou waarschijnlijk randgevallen missen. Bovendien, het gebruik van de standaardbibliotheek betekent minder code om te onderhouden.

Leuk feitje: Het oude ASCII had 32 als het magische nummer dat gevallen scheidde—voeg toe of trek 32 af om tussen 'A' en 'a' te springen. Met Unicode, niet zo eenvoudig.

Alternatieven? Bibliotheken. Voor de moderne C-programmeur transformeren bibliotheken zoals GLib tekenreeksen in een oogwenk, waarbij UTF-8 en dergelijke worden afgehandeld, maar dat is overkill voor ASCII-tekenreeksen.

## Zie Ook

- C Standaardbibliotheek Referentie: <http://www.cplusplus.com/reference/cctype/tolower/>
- ASCII Tabel en Beschrijving: <https://www.asciitable.com/>
- GLib Unicode manipulatie: <https://developer.gnome.org/glib/stable/glib-Unicode-Manipulation.html>
