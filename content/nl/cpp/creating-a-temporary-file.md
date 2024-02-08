---
title:                "Een tijdelijk bestand aanmaken"
aliases:
- nl/cpp/creating-a-temporary-file.md
date:                  2024-01-28T21:58:03.484796-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tijdelijk bestand aanmaken"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/cpp/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het creëren van een tijdelijk bestand betekent het maken van een bestand dat is ontworpen om tijdelijk gegevens op te slaan en wordt verwijderd na gebruik. Programmeurs doen dit om tussenliggende gegevens te verwerken zonder het bestandssysteem te vervuilen of het risico te lopen op conflicten met andere bestanden.

## Hoe:

Hier is hoe je een tijdelijk bestand in huidige C++ creëert en gebruikt:

```C++
#include <cstdio>
#include <filesystem>
#include <iostream>

int main() {
    // Creëer een uniek tijdelijk bestand met behulp van de filesystem bibliotheek
    std::filesystem::path temp_path = std::filesystem::temp_directory_path() /= std::tmpnam(nullptr);

    // Open het tijdelijke bestand
    std::FILE* temp_file = std::fopen(temp_path.c_str(), "w+");
    if (!temp_file) {
        std::perror("Bestand openen mislukt");
        return EXIT_FAILURE;
    }

    // Schrijf er iets naar toe
    std::fputs("Hallo, Tijdelijke Wereld!\n", temp_file);

    // Vergeet niet altijd het bestand te sluiten
    std::fclose(temp_file);

    // Toon het pad naar ons tijdelijke bestand
    std::cout << "Tijdelijk bestand gecreëerd op: " << temp_path << std::endl;

    // Opruiming: verwijder het tijdelijke bestand
    std::filesystem::remove(temp_path);

    return EXIT_SUCCESS;
}
```

Voorbeelduitvoer (het daadwerkelijke pad kan variëren):

```
Tijdelijk bestand gecreëerd op: /tmp/abc123
```

## Diepere Verkenning

Tijdelijke bestanden komen goed van pas in gevallen zoals het opslaan van de staat, het sorteren van grote datasets, of het afhandelen van output die niet hoeft te blijven bestaan. Historisch gezien werden temp bestanden gecreëerd in een algemene directory (zoals `/tmp` op Unix systemen) met een eenvoudige naamgevingsschema, met kans op botsingen. Modern C++ gebruikt de `<filesystem>` bibliotheek om dergelijke problemen te vermijden.

Alternatieven zijn het gebruik van RAM-gebaseerde tijdelijke opslag (zoals tmpfs in de meeste Unix-achtige systemen) of database blobs. Deze methoden houden de efemere gegevens in geheugen of beheerde systemen, wat de I/O overhead vermindert en de prestaties verbetert.

Wat betreft implementatie, onthoud dat:
- Bestands-I/O kan falen, dus controleer altijd je bestandsoperaties op fouten.
- Sluit altijd je bestanden om bronlekkage te voorkomen.
- Ruim op: Verwijder je tijdelijke bestanden (hoewel het systeem dit vaak doet, het is een goede gewoonte).

## Zie Ook

- [C++ Bestandssysteem Bibliotheek](https://en.cppreference.com/w/cpp/filesystem)
- [C++ IOstromen Bibliotheek](https://en.cppreference.com/w/cpp/io)
- [Tijdelijk Bestand Beheer in C](http://www.cplusplus.com/reference/cstdio/tmpfile/)
