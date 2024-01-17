---
title:                "Arbeta med CSV-filer"
html_title:           "C++: Arbeta med CSV-filer"
simple_title:         "Arbeta med CSV-filer"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

# Vad & Varför?
CSV står för "comma separated values" och är en typ av filformat som används för att lagra tabelldata. Det är ett populärt sätt att dela och utbyta data mellan olika program och plattformar. Programmerare använder ofta CSV-filer för att hantera och analysera stora mängder data på ett enkelt och effektivt sätt.

# Hur man gör:
För att arbeta med CSV-filer i C++, måste du först inkludera biblioteket ```<fstream>```.
Sedan kan du öppna CSV-filen med ```ifstream``` och läsa in data antingen rad för rad eller kolumn för kolumn.
Här är ett exempel på hur man kan läsa in en CSV-fil och skriva ut innehållet:

```C++
#include <iostream>
#include <fstream>

int main()
{
    // Öppna CSV-fil för läsning
    std::ifstream infile("min_data.csv");

    // Variabel för att lagra inlästa värden
    int value;

    // Läs in värden och skriv ut dem
    while (infile >> value)
    {
        std::cout << "Värde: " << value << std::endl;
    }

    return 0;
}

```

Exempel på innehåll i en CSV-fil:

```csv
1,2,3,4,5
6,7,8,9,10
```

Utmatning från exemplet ovan:

```text
Värde: 1
Värde: 2
Värde: 3
Värde: 4
Värde: 5
Värde: 6
Värde: 7
Värde: 8
Värde: 9
Värde: 10
```

# Djupdykning:
CSV-filer har funnits sedan 1970-talet och är fortfarande ett vanligt sätt att strukturera och dela data. Det finns dock några alternativ till CSV, som JSON och XML, som är mer lämpade för webbapplikationer. När du arbetar med CSV är det viktigt att kontrollera indata för att undvika felaktigheter eller datahaverier.

När du arbetar med CSV-filer är det också viktigt att känna till att varje rad representerar en datapost och att varje kolumn representerar ett attribut eller en egenskap för den datan. Det är viktigt att ha koll på rad- och kolumnseparatorer för att kunna läsa in datan på rätt sätt.

# Se även:
- [std::ifstream reference](https://www.cplusplus.com/reference/fstream/ifstream/)
- [CSV format](https://en.wikipedia.org/wiki/Comma-separated_values)