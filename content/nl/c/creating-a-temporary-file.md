---
title:                "Een tijdelijk bestand aanmaken"
date:                  2024-01-28T21:58:24.263277-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tijdelijk bestand aanmaken"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het aanmaken van een tijdelijk bestand in C biedt je een kladblok voor gegevensverwerking. Het is een manier om gegevens op te slaan die je nodig hebt tijdens de uitvoering van het programma, maar niet nadat het is beëindigd.

## Hoe:

C heeft functies zoals `tmpfile()` en `mkstemp()` om tijdelijke bestanden te maken. Hier is een voorbeeld van `tmpfile()`:

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp) {
        fputs("Schrijf iets tijdelijks.", temp);
        // Gebruik het bestand...
        rewind(temp); // Ga terug naar het begin om te lezen wat we geschreven hebben.
        
        // Laten we zeggen dat we het willen weergeven:
        char buffer[100];
        while (fgets(buffer, sizeof(buffer), temp) != NULL) {
            printf("%s", buffer);
        }
        // Sluit en verwijder automatisch wanneer het programma eindigt
        fclose(temp);
    } else {
        perror("tmpfile() mislukt");
    }

    return 0;
}
```
Voorbeelduitvoer: `Schrijf iets tijdelijks.`

## Diepgaande Duik
Tijdelijke bestanden bestaan al sinds de dageraad van moderne besturingssystemen. Ze zijn handig voor het omgaan met grote gegevens die niet in het geheugen passen, voor interprocescommunicatie, of voor vertrouwelijkheid (aangezien ze meestal worden verwijderd wanneer het programma eindigt).

`tmpfile()` creëert een uniek tijdelijk bestand in binaire lees-/schrijfmodus (`w+b`). Het bestand wordt automatisch verwijderd wanneer het gesloten is of het programma eindigt. Onthoud echter, aangezien het bestand in binaire modus geopend is, als je met tekst werkt, worden conversies voor nieuwe regeltekens niet automatisch afgehandeld.

Als je meer controle nodig hebt, gebruik dan `mkstemp()`. Het vervangt sjabloonkarakters in je bestandsnaam met een unieke reeks, en je moet het bestand handmatig verwijderen als je klaar bent.

```c
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

int main() {
    char template[] = "/tmp/mijntemp.XXXXXX";
    int fd = mkstemp(template);
    if (fd == -1) {
        perror("mkstemp() mislukt");
        exit(EXIT_FAILURE);
    }

    // Zet bestandsdescriptor om naar FILE object
    FILE *temp = fdopen(fd, "w+");
    if (temp == NULL) {
        perror("fdopen() mislukt");
        close(fd);
        exit(EXIT_FAILURE);
    }

    fputs("Hier is meer controle over temp bestanden.", temp);
    
    // Opruimen: Sluiten en handmatig verwijderen
    fclose(temp); 
    unlink(template); // Verwijder het bestand

    return 0;
}
```
Voorbeelduitvoer: (Geen expliciete uitvoer, maar een tijdelijk bestand wordt gecreëerd en verwijderd)

Waarom niet gewoon je eigen temp bestand rollen met `fopen()`? Het risico op botsingen. Onthoud, `tmpfile()` en `mkstemp()` zorgen ervoor dat de bestandsnaam uniek is om botsingen te voorkomen.

## Zie Ook

- C Standard Library documentatie: https://en.cppreference.com/w/c/io
- GNU C Library handleiding voor File System Interface: https://www.gnu.org/software/libc/manual/html_node/File-System-Interface.html
- Secure Coding in C en C++ voor het veilig omgaan met bestanden en gegevens: https://www.securecoding.cert.org
