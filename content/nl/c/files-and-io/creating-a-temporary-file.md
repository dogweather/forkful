---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:25.757161-07:00
description: "Een tijdelijk bestand in C cre\xEBren betekent het genereren van een\
  \ bestand dat bedoeld is om voor een korte duur gebruikt te worden, meestal als\
  \ kladruimte\u2026"
lastmod: 2024-02-19 22:05:10.400556
model: gpt-4-0125-preview
summary: "Een tijdelijk bestand in C cre\xEBren betekent het genereren van een bestand\
  \ dat bedoeld is om voor een korte duur gebruikt te worden, meestal als kladruimte\u2026"
title: Een tijdelijk bestand aanmaken
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tijdelijk bestand in C creëren betekent het genereren van een bestand dat bedoeld is om voor een korte duur gebruikt te worden, meestal als kladruimte voor gegevensverwerking of opslag. Programmeurs doen dit om tijdelijke gegevens te beheren zonder de permanente opslag van het programma te beïnvloeden of om ervoor te zorgen dat gevoelige gegevens worden gewist na gebruik.

## Hoe te:
Het creëren van een tijdelijk bestand in de C-programmeertaal kan functies zoals `tmpfile()` en `mkstemp()` gebruiken.

**Gebruikmakend van `tmpfile()`**: Deze functie creëert een uniek tijdelijk bestand dat automatisch wordt verwijderd wanneer het programma wordt beëindigd of het bestand wordt gesloten.

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp == NULL) {
        perror("Mislukt om tijdelijk bestand te creëren");
        return 1;
    }

    // Gegevens naar het tijdelijke bestand schrijven
    fputs("Dit is een test.\n", temp);

    // Terugspoelen en lezen wat we hebben geschreven
    rewind(temp);
    char buffer[1024];
    while (fgets(buffer, sizeof(buffer), temp) != NULL) {
        printf("%s", buffer);
    }

    // Automatisch verwijderd bij sluiten of afsluiten van programma
    fclose(temp);

    return 0;
}
```
**Voorbeelduitvoer:**
```
Dit is een test.
```

**Gebruikmakend van `mkstemp()`**: Biedt meer controle over de locatie van het tijdelijke bestand en de permissies. Het vereist een sjabloonstring die eindigt op `XXXXXX` die het dan vervangt met een unieke reeks om naamconflicten te voorkomen.

```c
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int main() {
    char sjabloon[] = "/tmp/mijntemp-XXXXXX";
    int fd = mkstemp(sjabloon);

    if (fd == -1) {
        perror("Mislukt om tijdelijk bestand te creëren");
        return 1;
    }
    
    printf("Tijdelijk bestand gecreëerd: %s\n", sjabloon);

    // Tijdelijke bestanden gecreëerd met mkstemp() moeten handmatig verwijderd worden
    unlink(sjabloon);

    close(fd);
    return 0;
}
```
**Voorbeelduitvoer:**
```
Tijdelijk bestand gecreëerd: /tmp/mijntemp-abc123
```

## Diepgaand onderzoek
Het concept van tijdelijke bestanden is niet uniek voor C, maar is een gemeenschappelijke functionaliteit in veel programmeeromgevingen vanwege het nut voor het afhandelen van vluchtige gegevens. De functie `tmpfile()`, gestandaardiseerd in de ISO C-norm, creëert een bestand met een unieke naam in een standaard directory, maar het bestaan is vluchtig, waardoor het ideaal is voor beveiligde of tijdelijke operaties.

Een opvallende beperking van `tmpfile()` is de afhankelijkheid van de standaard tijdelijke directory, die mogelijk niet geschikt is voor alle toepassingen, vooral in termen van permissies of beveiliging. In tegenstelling, `mkstemp()` maakt het specificeren van de directory mogelijk en zorgt voor een veilige bestandscreatie met gegarandeerde unieke bestandsnamen door de opgegeven sjabloonstring te wijzigen, en biedt een veelzijdigere oplossing ten koste van handmatig bestandsbeheer.

Het creëren van tijdelijke bestanden kan echter veiligheidskwetsbaarheden introduceren, zoals racecondities, als het niet correct wordt afgehandeld. Bijvoorbeeld, `tmpfile()` en `mkstemp()` richten zich op verschillende aspecten van veilige tijdelijke bestandscreatie (automatische verwijdering en veilige naamgeneratie, respectievelijk), maar geen van beide is een wondermiddel. Ontwikkelaars moeten rekening houden met de specifieke beveiligingsbehoeften van hun applicatie, inclusief mogelijke kwetsbaarheden die geïntroduceerd worden door tijdelijke bestanden, en moeten mogelijk extra beveiligingsmaatregelen implementeren buiten wat deze functies bieden.

In het bredere landschap van programmeren kunnen alternatieven zoals in-memory opslag (bijv. gebruikmakend van dynamische datastructuren of geheugen-toegewezen bestanden) betere prestaties of beveiliging bieden voor de afhandeling van tijdelijke gegevens. Desalniettemin blijven fysieke tijdelijke bestanden een essentieel hulpmiddel in veel scenario's, vooral voor grote gegevenssets of wanneer interprocescommunicatie betrokken is.
