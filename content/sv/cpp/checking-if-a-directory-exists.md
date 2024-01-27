---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-19
html_title:           "Arduino: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog (directory) finns innebär att du i din kod ser efter om en viss filväg leder till en existerande katalog. Det är användbart för att säkerställa att dina filoperationer, som läsning och skrivning, inte misslyckas på grund av att katalogen inte finns.

## How to: Så här gör du:
Här är ett kort exempel som använder `<filesystem>` för att kolla om en katalog finns:

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::filesystem::path dir = "/path/to/directory";

    if(std::filesystem::exists(dir) && std::filesystem::is_directory(dir)) {
        std::cout << "Katalogen finns." << std::endl;
    } else {
        std::cout << "Katalogen finns inte." << std::endl;
    }

    return 0;
}
```

Körningssvar beroende på om `/path/to/directory` finns:
```
Katalogen finns.
```
eller
```
Katalogen finns inte.
```

## Deep Dive: Fördjupning
I gamla tider, före `<filesystem>` blev en del av standardbiblioteket i C++17, var vi hänvisade till plattformsspecifika API:er eller tredjepartsbibliotek som `boost::filesystem` för att utföra systemoperationer. `<filesystem>` abstraherar bort många av de plattformsspecifika detaljerna och ger en enhetlig gränssnitt för att arbeta med filer och kataloger. 
Alternativ för att kontrollera om en katalog finns inkluderar:

- Användning av `stat` på UNIX-liknande system.
- Användning av `GetFileAttributes` på Windows.

Dessa metoder kräver mer plattformsberoende kod och är mindre intuitiva att använda. Med `<filesystem>`, blir denna operation enkel och portabel.

Implementationen av `<filesystem>` är trogen det övergripande paradigmet i standardbiblioteket, vilket är att ge starka garantier, såsom undantagshantering och resurshantering.

## See Also: Se Även
- [Filesystem library](https://en.cppreference.com/w/cpp/filesystem)
- [Boost.Filesystem](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)
- Tutorial for legacy methods: [POSIX `stat`](https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html), [Windows `GetFileAttributes`](https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileattributesa)
