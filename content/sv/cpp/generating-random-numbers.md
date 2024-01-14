---
title:    "C++: Generering av slumpmässiga tal"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Varför
Att generera slumpmässiga tal är en vanlig uppgift inom programmering. Det kan vara användbart för spel, simuleringar, och många andra applikationer som kräver ett element av slumpmässighet. I den här blogginlägget kommer vi att utforska hur man kan använda C++ för att skapa slumpmässiga tal.

## Hur man gör det
För att generera slumpmässiga tal använder vi oss av biblioteket `<random>` i C++. Det finns olika sätt att generera slumpmässiga tal beroende på våra behov, men det grundläggande konceptet är detsamma. Här är ett enkelt exempel på hur man kan skapa ett slumpmässigt tal inom ett visst intervall med hjälp av `std::uniform_int_distribution`:

```C++
#include <iostream>
#include <random>

int main()
{
    std::random_device rd; // skapar en lågkvalitativ slumpmässig seed
    std::mt19937 gen(rd()); // skapar en mer kvalitativ seed med hjälp av rd
    std::uniform_int_distribution<> dist(1, 10); // skapar en distribution från 1 till 10

    // genererar och skriver ut ett slumpmässigt tal
    std::cout << "Slumpmässigt tal: " << dist(gen) << std::endl;
    return 0;
}
```

Resultatet kan variera varje gång vi exekverar programmet, vilket är en indikator på att vårt slumpmässiga tal har genererats framgångsrikt.

## Djupdykning
Biblioteket `<random>` erbjuder även andra distributionsfunktioner som `std::uniform_real_distribution` för att generera slumpmässiga decimaltal och `std::normal_distribution` för att skapa tal baserade på en normalfördelning. Dessutom kan vi ändra seed-värdet för att få en annan sekvens av slumpmässiga tal.

Det finns också andra sätt att generera slumpmässiga tal utan att använda bibliotek. Till exempel kan vi använda oss av `rand()` funktionen som finns i standardbiblioteket `<cstdlib>` för enkla slumpmässiga tal. Men det är viktigt att notera att `rand()` inte är lika pålitlig som biblioteket `<random>` när det gäller att skapa slumpmässiga tal.

## Se även
- [Förstå random-funktionen i C++](https://www.tutorialspoint.com/cplusplus/cpp_random_function.htm)
- [Slumpmässiga tal i C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/#:~:text=rand()%20is%20used%20to%20generate%20a%20random%20number%20everytime.,seed%20value%20using%20srand()%20function.)