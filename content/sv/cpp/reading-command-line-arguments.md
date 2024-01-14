---
title:    "C++: Läsning av kommandoradsargument"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Varför

Att läsa command line arguments är en viktig del av C++ programmering och kan öka effektiviteten och funktionaliteten i dina program. Det är också en viktig färdighet att ha för att kunna utveckla avancerade och interaktiva applikationer.

# Hur man gör det

Att läsa command line arguments är relativt enkelt i C++. Du kan göra detta genom att använda variabeln argc som representerar antalet argument som skickas till ditt program och variabeln argv som innehåller själva argumenten. För att använda dessa variabler behöver du inkludera header-filen "cstdlib" och sedan definiera main-funktionen med två parametrar som ser ut så här:

`int main(int argc, char *argv[])`

För att sedan läsa argumenten, kan du använda en loop som går igenom alla argumenten och skriver ut dem tillsammans med deras position i argumentlistan. Här är ett exempel med några kommandon och deras output:

```C++
#include <cstdlib>
#include <iostream>

using namespace std;

int main(int argc, char *argv[]) {

    // Skriver ut antalet argument
    cout << "Antal arguments: " << argc << endl;

    // Loopar genom alla argument och skriver ut dem en efter en
    for (int i = 0; i < argc; i++) {
        cout << "Argument " << i+1 << ": " << argv[i] << endl;
    }

    return 0;
}
```

**Kommando:** `./program argument1 argument2`

**Output:**
```
Antal arguments: 3
Argument 1: ./program
Argument 2: argument1
Argument 3: argument2
```

Det är också möjligt att läsa in argument som nummer eller andra datatyper. För att göra detta behöver du använda en string stream. Här är ett exempel på hur man kan konvertera och spara argumentet som en int:

```C++
#include <cstdlib>
#include <iostream>
#include <sstream>

using namespace std;

int main(int argc, char *argv[]) {

    // Skapar en string stream och läser in argumentet som ett nummer
    stringstream convert;
    convert << argv[1];

    int num;
    convert >> num;

    // Skriver ut det konverterade numret och dess dubbla värde
    cout << num << " dubblerat blir: " << num * 2 << endl;

    return 0;
}
```

**Kommando:** `./program 5`

**Output:**
```
5 dubblerat blir: 10
```

# Djupdykning

Det finns många sätt att använda command line arguments i C++. Du kan till exempel använda flaggor för att aktivera olika funktioner i ditt program baserat på användarens input, eller utföra specifika åtgärder beroende på vilka argument som matas in.

Det är också viktigt att komma ihåg att det finns vissa begränsningar när det kommer till command line arguments i C++. Till exempel finns det ett maxantal argument som kan skickas till ett program och detta värde varierar beroende på systemet.

# Se även

- [C++ Command Line Arguments Tutorial](https://www.programiz.com/cpp-programming/command-line-arguments)
- [Official C++ Documentation on Command Line Parameters](https://docs.microsoft.com/en-us/cpp/c-runtime-library/parsing-the-command-line?view=msvc-160)