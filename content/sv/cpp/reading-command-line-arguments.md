---
title:                "Läsning av kommandoradsargument"
html_title:           "C++: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Varför

Om du någonsin har använt ett program som kräver dig att ange kommandon eller parametrar via kommandoraden, har du använt funktionen för att läsa kommandoraden i din kod. Detta gör det möjligt för användaren att anpassa programmet efter sina behov, vilket är viktigt för ett smidigt och användarvänligt program.

# Hur man gör

För att läsa kommandoraden i C++ behöver du inkludera `#include <iostream>` och `#include <stdlib.h>` i din kod. Därefter kan du använda `argc` och `argv` variablerna för att ta emot antalet argument som har skickats och en array med själva argumenten.

```C++
int main(int argc, char *argv[]) {
    // Kod för att hantera kommandoraden
}
```

För att komma åt de faktiska argumenten kan du använda index i `argv` arrayen. Till exempel om användaren skriver in `./program argument1 argument2`, då är `argv[0]` värdet `./program`, `argv[1]` är `argument1` och `argv[2]` är `argument2`.

Nu när du har läst in kommandoraden, kan du använda `if`-satser eller `switch`-satser för att hantera olika kommandon och parametrar som användaren kan ha skickat. Ett exempel på detta kan se ut som följande:

```C++
if (argc == 2 && strcmp(argv[1], "help") == 0) {
    // Kod för att visa hjälpmeddelande
}

else if (argc == 3 && strcmp(argv[1], "add") == 0) {
    int num1 = atoi(argv[2]);
    int num2 = atoi(argv[3]);
    int sum = num1 + num2;
    std::cout << "Summan av " << num1 << " och " << num2 << " är " << sum << std::endl;
}
```

Om användaren skriver in `./program help` kommer hjälpmeddelandet att visas och om användaren skriver in `./program add 5 10`, kommer summan av 5 och 10 att beräknas och visas på skärmen.

# Djupdykning

Förutom att läsa in kommandoraden har du också möjlighet att läsa in miljövariabler som kan ha skickats till programmet via kommandoraden. Du kan använda funktionen `getenv()` för att läsa in dessa variabler och sedan använda dem i din kod.

En annan viktig punkt att komma ihåg är att `argv` arrayen alltid kommer att ha minst ett element, även om ingen kommandorad har skickats. Detta första element kommer alltid att vara programmets namn. Därför är det viktigt att ha detta i åtanke när du hanterar kommandoraden i din kod.

# Se även

- [Dokumentation för `argc` och `argv`](https://www.cplusplus.com/articles/CommandLine/)
- [Guide för att läsa kommandoraden i C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Exempelkod för att läsa kommandoraden](https://www.dreamincode.net/forums/topic/139987-reading-command-line-arguments/)