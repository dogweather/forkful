---
title:                "Arbeta med CSV"
html_title:           "C++: Arbeta med CSV"
simple_title:         "Arbeta med CSV"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV, eller "Comma Separated Values", är en vanlig filformat som används för att lagra och hantera data i tabellform. Det är ofta användbart för att hantera stora mängder data och för att dela och överföra information mellan olika program. Om du är intresserad av att lära dig hur man arbetar med CSV-filer, kan det hjälpa dig att bli mer effektiv i ditt programmeringsarbete.

## Så här gör du

Om du vill arbeta med CSV-filer i C++, behöver du ett bibliotek som stödjer detta filformat. Ett populärt bibliotek som används för att läsa och skriva CSV-filer är "csv-parser". För att använda detta bibliotek i ditt C++-projekt, behöver du först installera det genom att använda verifieraren "vcpkg" eller via din operativsystems pakethanterare.

Efter installationen kan du börja använda biblioteket i ditt projekt. För att läsa en CSV-fil kan du använda följande kod:

```C++
#include <iostream>
#include "csv.h"

int main() {
    // Ladda csv-filen
    io::CSVReader<3> in("exempel.csv");
    in.read_header(io::ignore_extra_column, "Namn", "Ålder", "Stad");

    // Loopa igenom dataraden för rad
    std::string namn; int ålder; std::string stad;
    while(in.read_row(namn, ålder, stad)){
        std::cout << "Person: " << namn << "; Ålder: " << ålder << "; Stad: " << stad << std::endl;
    }
    return 0;
}
```

I detta exempel använder vi biblioteket för att läsa en CSV-fil med tre kolumner: Namn, Ålder och Stad. För varje rad i filen skrivs sedan informationen ut på skärmen.

Om du istället vill skriva till en CSV-fil kan du använda följande kod:

```C++
#include "csv.h"

int main() {
    // Skapa csv-fil
    io::CSVWriter csv("ny_fil.csv");

    // Skriv till filen rad för rad
    csv << "Person" << "Ålder" << "Stad";
    csv << "Anna" << 25 << "Stockholm";
    csv << "David" << 30 << "Göteborg";

    return 0;
}
```

Kom ihåg att inkludera "csv.h"-filen i ditt projekt och att ange antal kolumner för att skapa rätt typ av "csv-reader" eller "csv-writer".

## Deep Dive

CSV-filer kan vara användbara för en mängd olika ändamål, men de kan också vara knepiga att hantera om du inte är van vid att arbeta med dem. En viktig sak att komma ihåg är att varje rad i en CSV-fil representerar en datarad. Varje data skiljs åt med ett komma, därav namnet "Comma Separated Values".

Det är också viktigt att notera att en del problem kan uppstå när du arbetar med CSV-filer, särskilt om datat innehåller citattecken eller specialtecken. Som en försiktighet är det bäst att kontrollera datat noggrant innan du använder det för att undvika eventuella problem.

Om du behöver arbeta med CSV-filer på en mer avancerad nivå, finns det också andra bibliotek som du kan utforska, som till exempel "libcsv" eller "Fast C++ CSV Parser".

## Se även

Här är några användbara länkar som kan hjälpa dig att lära dig mer om att arbeta med CSV i C++:

- [csv-parser på GitHub](https://github.com/csv-parser/csv-parser)
- [en tutorial om att läsa CSV-filer i C++](https://www.analyticsvidhya.com/blog/2021/06/csv-handling-in-c-introduction-and-usage-with-examples/)
- [en guide om att skriva CSV-filer i C++](https://www.codegrepper.com/code-examples/cpp/cpp+write+csv)