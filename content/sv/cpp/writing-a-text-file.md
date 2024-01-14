---
title:    "C++: Skriva en textfil"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil är ett viktigt koncept inom programmering eftersom det tillåter oss att spara information på ett permanent sätt. Det kan vara användbart för att spara data eller för att skapa textbaserade rapporter. 

## Hur man gör

För att skriva en textfil i C++ behöver vi först inkludera <fstream> biblioteket. Därefter öppnar vi en instans av ofstream-klassen och använder dess funktion open() för att öppna en fil. Här är ett exempel:

```C++
#include <iostream>
#include <fstream>

int main() {
    std::ofstream file;
    // Skapa en ny fil med namnet "minFil.txt"
    file.open("minFil.txt");

    // Skriv en sträng till filen
    file << "Det här är en textfil!" << std::endl;

    // Stäng filen
    file.close();
    
    return 0;
}
```

Om vi nu öppnar "minFil.txt" i en textredigerare kommer vi att se att vår sträng har skrivits till filen.

## Fördjupning

Om vi vill lägga till mer text i vår fil utan att skriva över det som redan finns där, kan vi istället använda funktionen `fstream::app` (append) när vi öppnar filen. Detta kommer att lägga till allt nytt innehåll i slutet av filen istället för att skriva över det befintliga.

En annan användbar funktion är `fstream::in`, vilket betyder att vi endast har läsåtkomst till filen. Detta kan vara användbart om vi bara vill läsa in data från en fil istället för att skriva till den.

Det finns många fler funktioner och inställningar som kan användas när man arbetar med textfiler i C++. Det är viktigt att läsa på dokumentationen för att förstå dessa och använda dem på rätt sätt.

## Se även

- [C++ filhantering (cplusplus.com)](https://www.cplusplus.com/doc/tutorial/files/)
- [Input/output with files in C++ (GeeksforGeeks)](https://www.geeksforgeeks.org/input-output-with-files-in-cpp/)