---
title:                "Een tekstbestand lezen"
date:                  2024-01-28T22:05:01.529804-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand lezen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tekstbestand lezen gaat over het toegang krijgen tot de gegevens van het bestand als tekenreeksen, teken voor teken of regel voor regel. Programmeurs doen dit om opgeslagen informatie te verwerken, te analyseren of te manipuleren zonder elke keer handmatige invoer.

## Hoe:

Laten we een tekstbestand lezen. We openen het, lezen eruit en sluiten het. Basiszaken.

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file;
    char bestandsnaam[] = "voorbeeld.txt";
    char ch;

    file = fopen(bestandsnaam, "r"); // Open het bestand in leesmodus

    if (file == NULL) {
        perror("Fout bij het openen van het bestand.\n");
        exit(EXIT_FAILURE);
    }

    printf("Inhoud van %s:\n", bestandsnaam);

    while ((ch = fgetc(file)) != EOF) { // Lees en print teken voor teken
        putchar(ch);
    }

    fclose(file); // Sluit het bestand

    return 0;
}
```

Als we ervan uitgaan dat `voorbeeld.txt` "Hallo, C!" bevat, zal de uitvoer zijn:
```
Inhoud van voorbeeld.txt:
Hallo, C!
```

## Diepgaande Duik

Terug in de jaren 70 werd C geboren, en daarmee de manier waarop we vandaag de dag bestanden lezen. Het is geen raketwetenschap, maar er zijn nuances. Je gebruikt `fopen` om bestanden te openen en `fgetc` om één teken tegelijk te lezen. Maar waarom teken voor teken? Je zou regels kunnen lezen met `fgets` of het hele bestand met `fread` als dat beter past bij jouw situatie. Het gaat allemaal om controle en wat jouw programma nodig heeft om te verwerken.

Achter de schermen zegt `fopen` tegen je besturingssysteem: "Hé, ik heb dit bestand nodig, geef me toegang!" En het systeem zegt oké door een `FILE` pointer terug te geven. De functie `fgetc` fluistert tegen de bestandspointer: "Geef me het volgende byte, wil je?" En dat doet het, totdat het EOF raakt, de marker voor het einde van het bestand.

Alternatieven? Zeker. Je hebt `fscanf` voor geformatteerd lezen, `getline` voor de moderne jongens, of low-level `read` systeemaanroepen als je dicht bij de hardware wilt zijn. En vergeet niet, nadat het laatste byte is gelezen, wees beleefd en `fclose` het bestand.

## Zie Ook

Om dieper te duiken, bekijk deze:

- Documentatie van de C-standaardbibliotheek: [https://en.cppreference.com/w/c/io](https://en.cppreference.com/w/c/io)
- GNU C-bibliotheekreferentiehandleiding: [https://www.gnu.org/software/libc/manual/html_node/I_002fO-Overview.html](https://www.gnu.org/software/libc/manual/html_node/I_002fO-Overview.html)
- Meer leren over verschillende leesfuncties: [https://www.tutorialspoint.com/c_standard_library/c_function_fread.htm](https://www.tutorialspoint.com/c_standard_library/c_function_fread.htm)
- Voor de echt nieuwsgierigen, een diepe duik in de Linux systeemaanroepen: [https://man7.org/linux/man-pages/man2/read.2.html](https://man7.org/linux/man-pages/man2/read.2.html)
