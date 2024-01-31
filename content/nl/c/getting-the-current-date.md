---
title:                "Het huidige datum ophalen"
date:                  2024-01-28T22:01:10.300465-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het huidige datum ophalen"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

De huidige datum verkrijgen betekent uitzoeken wat de datum van vandaag is volgens de interne klok van het systeem. Programmeurs doen dit om logs te dateren, evenementen te valideren en gegevens van een tijd-stempel te voorzien.

## Hoe:

Je zult `time.h` willen includeren om met tijd in C om te gaan.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);
    
    printf("Huidige Datum: %02d-%02d-%d\n", tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    
    return 0;
}
```

Voorbeeld van uitvoer:
```
Huidige Datum: 15-04-2023
```

## Diepere Duik

Historisch gezien, gaat omgaan met tijd in C terug tot de vroege dagen van UNIX, dankzij C's sterke systeemniveau capaciteiten. Voor huidige data vertrouwen we op de `time.h` bibliotheek, die er is geweest sinds C gestandaardiseerd werd door ANSI.

Het `time_t` type slaat de huidige tijd sinds het Epoch (00:00:00 UTC op 1 januari 1970) op in seconden. De `localtime` functie vertaalt deze tijd naar een `struct tm` die de kalenderdatum en tijd houdt, opgesplitst in zijn componenten.

Alternatieven? Er zijn andere manieren om tijd in C te manipuleren en te representeren. Bijvoorbeeld, `gmtime` zet `time_t` om naar gecoördineerde universele tijd (UTC) in plaats van lokale tijd, wat `localtime` doet. Met `strftime`, kun je je datum- en tijdformaat uitgebreid aanpassen.

Wat de details betreft, `time_t` is typisch een geheel getal of een floating-point type. Implementatie kan variëren over systemen, maar de standaard eist niet het precieze type, alleen dat het in staat is tijden te representeren.

Wanneer je tijdgerelateerde functies gebruikt, onthoud dan om rekening te houden met zomertijd en lokale specifieke gegevens als je applicatie daar gevoelig voor is.

## Zie Ook

- The GNU C Library Reference Manual over Tijd: https://www.gnu.org/software/libc/manual/html_node/Time.html
- C Standaard Bibliotheek - time.h: https://en.cppreference.com/w/c/chrono
- Leer meer over tijdformaten met strftime: https://en.cppreference.com/w/c/chrono/strftime
