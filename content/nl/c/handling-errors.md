---
title:                "Fouten afhandelen"
date:                  2024-01-28T22:01:46.229217-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Foutafhandeling in C gaat over het verwachten van het onverwachte. Het voorkomt dat programma's op hol slaan wanneer ze tegen problemen aanlopen. Programmeurs doen dit om fouten op een elegante manier te hanteren en hun code betrouwbaar te houden.

## Hoe te:

Laten we eens kijken hoe we dit in C doen:

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("nonexistentfile.txt", "r");
    if (fp == NULL) {
        perror("Fout bij het openen van bestand");
        return EXIT_FAILURE;
    }
    // Doe iets met het bestand
    fclose(fp);
    return EXIT_SUCCESS;
}
```

Voorbeelduitvoer wanneer het bestand niet bestaat:
```
Fout bij het openen van bestand: Bestand of map bestaat niet
```

## Diepere Duik

In de vroege dagen van C was foutafhandeling basic - meestal retourcodes en handmatige controles. Voer `errno` in, een globale variabele die wordt bijgewerkt wanneer functies falen. Het is op zichzelf niet thread-safe, dus de nieuwere `strerror` en `perror` functies werden geïntroduceerd voor betere foutmeldingen.

Alternatieven? Modern C is niet beperkt tot `errno`. Er zijn setjmp en longjmp voor niet-lokale sprongen wanneer een ramp zich voordoet. Sommige mensen geven de voorkeur aan het definiëren van hun eigen foutcodes, terwijl anderen kiezen voor uitzonderingsachtige structuren in C++.

Implementatiedetails kunnen ingewikkeld zijn. Bijvoorbeeld, `errno` is thread-safe in POSIX conforme systemen door de magie van Thread Local Storage (TLS). In ingebedde systemen, waar middelen kostbaar zijn, kan een voorkeur uitgaan naar aangepaste foutafhandelingscode boven standaard aanpakken die de software zouden kunnen verzwaren.

## Zie ook

- Een gedetailleerde duik in `errno`: https://en.cppreference.com/w/c/error/errno
- Voor thread veiligheid, zie POSIX threads en errno: http://man7.org/linux/man-pages/man3/pthread_self.3.html
- Een introductie tot setjmp en longjmp: https://www.cplusplus.com/reference/csetjmp/
- Voor uitzonderingsafhandeling in C++, bekijk: https://isocpp.org/wiki/faq/exceptions
