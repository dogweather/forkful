---
title:    "C++: Kontrollera om en katalog finns (Titel på en artikel om datorprogrammering)"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp finns är en viktig del av att skriva effektiv C++ kod. Det kan hjälpa till att undvika potentiella fel och se till att ens programmets filsystem-funktioner fungerar som de ska.

## Hur man gör det

Först och främst måste man inkludera "cstdio" biblioteket. Sedan kan man använda funktionen "opendir ()" för att öppna den angivna mappen och om den inte finns så returnerar den "nullptr", vilket indikerar att mappen inte finns.

```C++
#include <cstdio>
...
DIR* dir = opendir("mappnamn");

if (dir == nullptr) {
    // göra något om mappen inte finns
} else {
    // göra något om mappen finns
}
```

## Fördjupning

För att kunna göra en mer djupgående kontroll av mappen kan man använda funktionen "stat ()". Denna funktion returnerar information om en viss fil eller mapp, såsom dess storlek, ägare och sist modifierade datum. Om mappen inte finns kommer "stat ()" att returnera "-1", vilket kan användas för att bekräfta att mappen inte finns.

```C++
#include <cstdio>
...
struct stat buffer;
if (stat("mappnamn", &buffer) == -1) {
    // mappen finns inte
} else {
    // mappen finns
}
```

## Se även

- [cppreference - opendir](https://en.cppreference.com/w/c/io/opendir)
- [cppreference - stat](https://en.cppreference.com/w/c/io/stat)