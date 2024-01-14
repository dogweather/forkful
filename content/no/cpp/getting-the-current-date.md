---
title:    "C++: Å få nåværende dato"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen er ofte en viktig del av å skrive programmer. Det kan være nyttig for å lage datostempel, eller for å overvåke og trekke ut data fra forskjellige dager.

## Hvordan

Det er flere måter å få tak i den nåværende datoen i C++. Den enkleste måten er å bruke standardbiblioteket, hvor vi kan bruke funksjonen `localtime` for å hente ut datoen i et strukturert format.

```C++
#include <iostream>
#include <ctime>

int main() {
    // Henter ut tiden
    time_t now = time(0);
    
    // Konverterer tiden til en lesbar streng
    char* dt = ctime(&now);
    
    // Skriver ut datoen
    std::cout << "Dagens dato er: " << dt << std::endl;
    
    return 0;
}
```

Output:

```
Dagens dato er: Mon Aug 23 14:28:42 2021
```

Vi kan også formatere datoen etter våre ønsker ved å bruke `strftime` funksjonen.

```C++
#include <iostream>
#include <ctime>

int main() {
    // Henter ut tiden
    time_t now = time(0);
    
    // Konverterer tiden til en struktur
    struct tm * timeinfo;
    timeinfo = localtime(&now);
    
    // Setter opp ønsket format
    char buffer[80];
    strftime(buffer, 80, "%d/%m/%Y", timeinfo);
    
    // Skriver ut datoen
    std::cout << "Dagens dato er: " << buffer << std::endl;
    
    return 0;
}
```

Output:

```
Dagens dato er: 23/08/2021
```

## Dykk ned

For å forstå hvordan disse funksjonene fungerer, er det viktig å ha kunnskap om `time_t` og `struct tm`. `time_t` er en datatype som inneholder informasjon om tiden, mens `struct tm` er en struktur som inneholder de forskjellige elementene av datoen som år, måned og dag.

`localtime` funksjonen tar inn en `time_t` verdi og konverterer den til en `struct tm` struktur. `ctime` funksjonen gjør om denne strukturen til en lesbar streng.

`strftime` funksjonen bruker et formatstring til å konvertere datoen til en tilpasset streng. Du kan finne en liste over formatalternativer [her](https://www.cplusplus.com/reference/ctime/strftime/).

## Se også

- [C++ time library](https://www.cplusplus.com/reference/ctime/)
- [C++ strftime function documentation](https://www.cplusplus.com/reference/ctime/strftime/)