---
date: 2024-01-20 17:39:59.998868-07:00
description: "How to: I C++, kan du bruke `<filesystem>` biblioteket for \xE5 h\xE5\
  ndtere midlertidige filer. Her er et eksempel."
lastmod: '2024-03-13T22:44:41.119573-06:00'
model: gpt-4-1106-preview
summary: "I C++, kan du bruke `<filesystem>` biblioteket for \xE5 h\xE5ndtere midlertidige\
  \ filer."
title: Opprette en midlertidig fil
weight: 21
---

## How to:
I C++, kan du bruke `<filesystem>` biblioteket for å håndtere midlertidige filer. Her er et eksempel:

```C++
#include <iostream>
#include <fstream>
#include <filesystem>

int main() {
    // Opprett en unik midlertidig fil
    std::filesystem::path temp_path = std::filesystem::temp_directory_path();
    std::filesystem::path temp_file = temp_path / "minMidlertidigFilXXXXXX";

    // Bruke tab, siden filesystem-API'en ikke direkte støtter opprettelse av midlertidige filer
    int fd = mkstemp(temp_file.data());
    // Sjekk om filen er opprettet
    if (fd == -1) {
        std::cerr << "Kunne ikke opprette midlertidig fil.\n";
        return 1;
    }

    // Skriv til midlertidig fil
    write(fd, "Hei fra C++\n", 13);

    // Lukk filen
    close(fd);

    std::cout << "Midlertidig fil opprettet på: " << temp_file << std::endl;

    // Til slutt, slett den midlertidige filen
    std::filesystem::remove(temp_file);
    
    return 0;
}
```

Resultatet vil være en midlertidig fil opprettet, brukt og deretter slettet, uten noe varig spor.

## Deep Dive
Opprettelse av midlertidige filer har vært viktig siden tidlige dager av programmering. Det hjelper med håndtering av cache, midlertidige backups, og som et skravleområde for store operasjoner. I eldre C++ versjoner eller i POSIX-systemer, brukte man ofte `tmpfile()` eller `mkstemp()`. Modern C++ har bedre verktøy i `<filesystem>` biblioteket som interagerer pent med filsystemet og gir mer kontroll.

En alternativ måte å håndtere midlertidig filoppretting er med tredjeparts biblioteker som `boost::filesystem` som har funksjonalitet lignende standard `std::filesystem`.

Når det gjelder implementasjon, være oppmerksom på sikkerhetsaspektene. Det er viktig å generere unike filnavn og unngå kollisjoner, noe som `std::filesystem` hjelper med. Pass også på å rydde opp og slette midlertidige filer for å forhindre søppeldata og potensielle sikkerhetsrisikoer.

## See Also
Besøk følgende lenker for mer informasjon:
- [std::filesystem dokumentasjon](https://en.cppreference.com/w/cpp/filesystem)
- [C++ File I/O handling med fstream](https://www.cplusplus.com/doc/tutorial/files/)
- [Sikkerhetsaspekter ved midlertidige filer](https://www.owasp.org/index.php/Insecure_Temporary_File)
