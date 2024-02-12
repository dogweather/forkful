---
title:                "Loggen"
aliases: - /nl/c/logging.md
date:                  2024-02-03T17:58:53.496563-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loggen"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/logging.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Loggen in C houdt in dat je de stroom en opmerkelijke gebeurtenissen van een programma tijdens de uitvoering registreert, waardoor een tastbare beoordeling van zijn gedrag en prestaties wordt geboden. Programmeurs gebruiken loggen voor debugdoeleinden, het monitoren van de softwaregezondheid en het waarborgen van de systeembeveiliging.

## Hoe te:

In C kan loggen worden bereikt met basisbestandsbewerkingen of met behulp van meer geavanceerde bibliotheken. Voor de eenvoud beginnen we met de standaard I/O-bibliotheek. De volgende snippets laten basisimplementaties van loggen zien.

Om eenvoudige berichten te loggen:

```c
#include <stdio.h>

int main() {
    FILE *logFile;
    logFile = fopen("application.log", "a"); // Open het logbestand in toevoegmodus
    
    if (logFile == NULL) {
        perror("Fout bij het openen van logbestand.");
        return -1;
    }
    
    fprintf(logFile, "Starten van applicatie.\n");
    
    // Je applicatielogica hier
    
    fprintf(logFile, "Applicatie succesvol beëindigd.\n");
    fclose(logFile);
    
    return 0;
}
```

Uitvoer in `application.log`:

```
Starten van applicatie.
Applicatie succesvol beëindigd.
```

Om meer gedetailleerde logs met tijdstempels en logniveaus te omvatten:

```c
#include <stdio.h>
#include <time.h>

void logMessage(FILE *logFile, const char* niveau, const char* bericht) {
    time_t nu;
    time(&nu);
    char* datumtijd = ctime(&nu);
    datumtijd[strlen(datumtijd)-1] = '\0'; // Verwijder de nieuwe regel karakter
    fprintf(logFile, "[%s] %s - %s\n", datumtijd, niveau, bericht);
}

int main() {
    FILE *logFile;
    logFile = fopen("detailed.log", "a");
    
    if (logFile == NULL) {
        perror("Fout bij het openen van logbestand.");
        return -1;
    }
    
    logMessage(logFile, "INFO", "Applicatie start");
    // Je applicatielogica hier
    logMessage(logFile, "ERROR", "Een voorbeeldfout");
    
    fclose(logFile);
    
    return 0;
}
```

Uitvoer in `detailed.log`:

```
[Thu Mar 10 14:32:01 2023] INFO - Applicatie start
[Thu Mar 10 14:32:02 2023] ERROR - Een voorbeeldfout
```

## Diepe Duik

Loggen in C, zoals gedemonstreerd, vertrouwt op eenvoudige bestandsbewerkingen, wat effectief is maar niet zo krachtig of flexibel als logvoorzieningen in andere talen, zoals Python's `logging` module of Java's `Log4j`. Voor meer geavanceerde logmogelijkheden in C wenden ontwikkelaars zich vaak tot bibliotheken zoals `syslog` op Unix-achtige systemen, die systeembrede logmanagement biedt, of externe bibliotheken zoals `log4c`.

Historisch gezien is loggen een integraal onderdeel van programmeren geweest, dat teruggaat tot vroege programmeerpraktijken waarbij het volgen en begrijpen van de programmastroom en fouten voornamelijk werd gedaan door middel van fysieke afdrukken. Naarmate systemen evolueerden, werd loggen geavanceerder, nu ondersteunend aan verschillende niveaus van ernst, logrotatie en asynchrone logging.

Hoewel de standaardbibliotheek van C de basishulpmiddelen biedt voor het implementeren van loggen, leiden de beperkingen vaak tot de creatie van aangepaste logframeworks of de adoptie van externe bibliotheken voor meer functierijke en flexibele logoplossingen. Ondanks deze beperkingen is het begrijpen en implementeren van basaal loggen in C cruciaal voor het debuggen en onderhouden van software, vooral in omgevingen waar externe afhankelijkheden tot een minimum beperkt moeten worden.
