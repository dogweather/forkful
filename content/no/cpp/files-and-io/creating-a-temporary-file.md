---
title:                "Opprette en midlertidig fil"
aliases:
- /no/cpp/creating-a-temporary-file/
date:                  2024-01-20T17:39:59.998868-07:00
model:                 gpt-4-1106-preview
simple_title:         "Opprette en midlertidig fil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Opprette en midlertidig fil betyr å skape en fil som er ment for kortvarig bruk og ofte slettet etter bruk. Programmerere gjør dette for å lagre data midlertidig uten å påvirke det permanente lagringssystemet.

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
