---
title:                "C++: Konvertera ett datum till en sträng"
simple_title:         "Konvertera ett datum till en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera ett datum till en sträng är en vanlig uppgift i en programmerares vardag. Det kan användas för att visa datum i ett användargränssnitt eller för att spara datum som en del av en fil eller databas. I denna bloggpost kommer jag att visa hur du enkelt kan konvertera ett datum till en sträng i C++.

## Hur man gör
För att konvertera ett datum till en sträng i C++ behöver du först inkludera <iostream> och <string> biblioteken. Sedan kan du använda funktionen std::to_string() för att konvertera ett heltal till en sträng. Du kan också använda funktionen std::tm för att representera ett datum och sedan använda std::put_time() för att formatera datumet till en sträng. Här är ett kodexempel:

```C++
#include <iostream>
#include <string>
#include <iomanip>
#include <ctime>

int main()
{
    // Konvertera datumet till en sträng med hjälp av std::tm och std::put_time
    std::time_t t = std::time(nullptr);
    std::tm* now = std::localtime(&t);
    std::string date = std::put_time(now, "%d/%m/%Y");

    std::cout << "Dagens datum är: " << date << std::endl;
    return 0;
}
```
Detta kodexempel kommer att skriva ut dagens datum i formatet DD/MM/YYYY.

## Djupdykning
Enligt C++11-standard tillhandahåller standardbiblioteket funktionen std::put_time() för att formatera ett std::tm-objekt (representativt för ett datum och en tid) till en sträng. Formatsträngen som används i detta exempel ("%d/%m/%Y") är en specifierare för datum som består av dag (DD), månad (MM) och år med fyra siffror (YYYY). Det finns också flera andra specifierare som du kan använda för att anpassa formatet på den genererade strängen.

## Se även
- [std::to_string()](https://www.cplusplus.com/reference/string/to_string/)
- [std::tm](https://www.cplusplus.com/reference/ctime/tm/)
- [std::put_time()](https://www.cplusplus.com/reference/iomanip/put_time/)