---
title:                "Kontrollera om en katalog existerar"
html_title:           "C++: Kontrollera om en katalog existerar"
simple_title:         "Kontrollera om en katalog existerar"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför:

Att kontrollera om en katalog finns är enkelt: det är bara att se om en viss mapp har skapats på din filsystem. Varför gör vi detta? För att undvika felmeddelanden eller systemkrascher när vi försöker ändra eller flytta till en mapp som inte finns.

## Så här gör du:

För jämna saker ut, låt oss dyka in i lite kod. Vi kommer att behöva följa `#include <filesystem>`. `std::filesystem::exists()` kommer att göra jobbet.

```C++
#include <iostream>
#include <filesystem>

int main() {
    if(std::filesystem::exists("/your/path")) {
        std::cout << "Katalogen finns!\n";
    } else {
        std::cout << "Katalogen finns inte.\n";
    }
    return 0;
}
```

Om katalogen finns kommer output vara "Katalogen finns!", annars kommer det vara "Katalogen finns inte."

## Djupdykning:

Dina alternativ är inte begränsade till `std::filesystem::exists()`. Första versionen av filsystem API tillägget kommer ifrån boost biblioteket före C++17. Ett annat alternativ är att använda `stat` systemanrop, men det kommer vara svårare att portera ditt program till andra plattformar. 

## Se Även: 

Vill du se mer? Kolla in dessa relaterade resurser:

- cppreference.com: [std::filesystem](https://en.cppreference.com/w/cpp/filesystem)
- boost.org: [Filesysteem Library](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)
- linux.die.net: [stat system call](https://linux.die.net/man/2/stat)