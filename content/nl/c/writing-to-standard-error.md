---
title:                "Schrijven naar standaardfout"
date:                  2024-01-28T22:13:21.788523-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar standaardfout (stderr) is hoe je foutmeldingen en diagnostiek uitvoert in C. Het is gescheiden van standaarduitvoer (stdout) om je reguliere en foutuitvoeren anders te behandelen, zoals het loggen van fouten of het stroomlijnen van debugging.

## Hoe te:

Hier is hoe je naar stderr kunt schrijven, met behulp van standaardbibliotheekfuncties.

```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Er is een fout opgetreden!\n");
    return 0;
}
```

Voorbeelduitvoer:

```
Er is een fout opgetreden!
```

Gebruik `perror` wanneer je een bericht wilt toevoegen over de laatste systeemfout:

```C
#include <stdio.h>
#include <errno.h>

int main() {
    fopen("nonexistentfile.txt", "r");

    if (errno) {
        perror("Bestand openen mislukt");
    }

    return 0;
}
```

Voorbeelduitvoer:

```
Bestand openen mislukt: Bestand of map bestaat niet
```

## Diepgaand

Historisch gezien helpt het scheiden van stderr van stdout bij het uitvoeren van programma's vanuit een shell. Standaarduitvoer kan worden omgeleid naar een bestand of een ander programma, terwijl standaardfout zichtbaar blijft in de terminal. Dit onderscheid is cruciaal in Unix-gebaseerde systemen.

Alternatieven voor `fprintf` of `perror` zijn onder meer direct schrijven naar de bestandsdescriptor, zoals `write(2, "Fout\n", 6);`, hoewel dit minder gebruikelijk is omdat het op een lager niveau is.

Wat implementatie betreft, is stderr een `FILE` pointer die gebufferd is. Maar, in tegenstelling tot stdout, is het meestal ingesteld op ongebufferde modus zodat foutmeldingen meer onmiddellijk zijn, wat cruciaal is voor het begrijpen van programmamissers terwijl ze gebeuren.

## Zie Ook

- [GNU C Bibliotheek: Standaard Streams](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- [Write Syscall Man Pagina](https://man7.org/linux/man-pages/man2/write.2.html)
