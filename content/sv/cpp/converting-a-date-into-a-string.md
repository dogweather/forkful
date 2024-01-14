---
title:    "C++: Omvandla ett datum till en sträng"
keywords: ["C++"]
---

{{< edit_this_page >}}

Varför: När man arbetar med datum kan det vara användbart att kunna konvertera ett datum till en sträng för att enklare hantera och skriva ut datumen.

Hur man gör: 
Så här använder man C++-funktionerna för att konvertera ett datum till en sträng:

```C++
#include <iostream>
#include <string>
#include <chrono>
#include <ctime>

using namespace std;

int main()
{
    // skapa ett datumobjekt med hjälp av "chrono" biblioteket
    chrono::system_clock::time_point today = chrono::system_clock::now();

    // konvertera datumet till ANSI-format genom att använda ctime-biblioteket
    time_t time = chrono::system_clock::to_time_t(today);

    // konvertera tiden till en sträng
    string dateStr = ctime(&time);

    // skriv ut datumet
    cout << "Dagens datum är: " << dateStr << endl;

    return 0;
}
```

Output:
```
Dagens datum är: Wed Apr 14 11:01:58 2021
```

Djupdykning: 
För att konvertera ett datum till en sträng kan man använda sig av olika bibliotek i C++. Istället för att manuellt formatera datumet till en sträng, kan man använda bibliotek som "chrono" och "ctime" för att underlätta processen. Genom att först skapa ett datumobjekt med hjälp av "chrono" biblioteket och sedan konvertera det till ANSI-format med "ctime", får man en standardiserad och läsbar sträng.

Se även: 
- [chrono bibliotek i C++](https://www.cplusplus.com/reference/chrono/)
- [ctime bibliotek i C++](https://www.cplusplus.com/reference/ctime/)
- [Konvertera datum till en sträng i C++](https://www.educative.io/edpresso/how-to-convert-a-date-to-a-string-in-cpp)