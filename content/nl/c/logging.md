---
title:                "Logboekregistratie"
date:                  2024-01-28T22:02:50.508414-07:00
model:                 gpt-4-0125-preview
simple_title:         "Logboekregistratie"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Loggen is in essentie het opschrijven van wat je programma doet, typisch door berichten naar een bestand of terminal uit te schrijven. Programmeurs doen dit om gebeurtenissen bij te houden, problemen te diagnosticeren en om een audittraject te hebben dat het verhaal van de werking van een applicatie over tijd vertelt.

## Hoe:
Laten we beginnen met enkele basisprincipes. C heeft geen ingebouwd logframework, maar je kunt iets eenvoudigs maken met `stdio.h`. Zo gaat het in zijn werk:

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t now;
    time(&now);
    char *date = ctime(&now);
    date[strlen(date) - 1] = '\0'; // Verwijder de nieuwe regel aan het einde van ctime()'s resultaat
    printf("[%s] %s\n", date, message);
}

int main() {
    logMessage("De applicatie is gestart.");
    // ... jouw code volgt hier ...
    logMessage("De applicatie doet iets belangrijks.");
    // ... jouw code gaat verder ...
    logMessage("De applicatie is beëindigd.");
    return 0;
}
```

Een voorbeeld van de uitvoer kan er zo uitzien:

```
[Tue Mar 9 12:00:01 2023] De applicatie is gestart.
[Tue Mar 9 12:00:02 2023] De applicatie doet iets belangrijks.
[Tue Mar 9 12:00:03 2023] De applicatie is beëindigd.
```

Natuurlijk zou je in de echte wereld waarschijnlijk naar een bestand willen schrijven in plaats van naar de terminal, verschillende logniveaus willen afhandelen en misschien een vooraf gedefinieerde bibliotheek gebruiken.

## Diepe Duik
Loggen in C heeft een eigen charme - het is net zo low-level als de meeste andere delen van de taal. Historisch gezien werd loggen uitgevoerd met `fprintf` met `stderr` of een bestandspointer. Naarmate programma's complexer werden, werden ook de logbehoeften ingewikkelder, wat leidde tot de ontwikkeling van bibliotheken zoals `syslog` op Unix-systemen, die het loggen van meerdere bronnen met verschillende niveaus van belangrijkheid konden afhandelen.

In het moderne landschap zijn er tal van C logbibliotheken beschikbaar, zoals `zlog`, `log4c` en `glog`, die een rijke reeks functies bieden, waaronder logrotatie, gestructureerd loggen en multithreaded loggen. Deze oplossingen bieden controle over de verbositeit, bestemmingen en formaten van logs.

Bij het implementeren van een logsysteem moeten details zoals tijdstempelformattering, logbestandsbeheer en prestaties in overweging worden genomen. Tijdstempels in logs zijn cruciaal voor het correleren van gebeurtenissen, terwijl logrotatie ervoor zorgt dat logbestanden niet te veel schijfruimte verbruiken. Het loggen moet ook snel en niet-blokkerend zijn voor de hoofdtoepassingsstroom om te voorkomen dat loggen een bottleneck wordt.

## Zie Ook
Voor meer verdieping in logbibliotheken en -praktijken in C, bekijk deze bronnen:

- GNU `syslog` handleiding: https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog`: Een zeer configureerbare logbibliotheek voor C - https://github.com/HardySimpson/zlog
- `log4c`: Een lograamwerk voor C gemodelleerd naar Log4j - http://log4c.sourceforge.net/
- `glog`: Google's applicatieniveau logbibliotheek - https://github.com/google/glog
