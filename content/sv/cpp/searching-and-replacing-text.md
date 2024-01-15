---
title:                "Söka och ersätta text"
html_title:           "C++: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför?

Att söka och ersätta text är en vanlig uppgift i programmering, särskilt när man arbetar med större mängder data eller textfiler. Genom att använda lämpliga sök- och ersättningsfunktioner kan man effektivt hitta och byta ut önskad text i en given kod eller fil.

## Så här gör du

För att söka och ersätta text i C++ finns det flera användbara funktioner tillgängliga. Nedan följer några exempel på hur man kan använda dessa funktioner i kod och vilken output man kan förvänta sig:

```C++
// Läser in en fil som en sträng
#include <iostream>
#include <string>
#include <fstream>

int main(){
    std::string file_content;
    std::ifstream input_file ("input.txt");

    if (input_file.is_open()){
        // Sparar innehållet i filen i en sträng
        file_content.assign(std::istreambuf_iterator<char>(input_file), std::istreambuf_iterator<char>());
        input_file.close();
    }

    // Söker efter förekomsten av "älgen" i filen och ersätter med "renen"
    size_t found = file_content.find("älgen");
    while(found != std::string::npos){
        file_content.replace(found, 5, "renen"); // 5 är längden på "älgen"
        found = file_content.find("älgen", found + 5); // Flyttar sökningen framåt
    }

    // Skriver det nya innehållet till en ny fil
    std::ofstream output_file ("output.txt");
    output_file << file_content;
    output_file.close();

    return 0;
}
```

I ovanstående exempel används funktionerna `find()` och `replace()` för att söka igenom en fil och ersätta viss text. Efter att ha läst in filen som en sträng med hjälp av `ifstream` klassen, används `find()` för att leta efter förekomsten av "älgen" i strängen. Om det hittas så används `replace()` för att byta ut det mot "renen". Sedan flyttas sökningen framåt med hjälp av `found` variabeln, så att inte samma förekomst ersätts flera gånger. Slutligen skrivs den nya strängen till en ny fil med hjälp av `ofstream` klassen.

En annan vanlig funktion som kan användas för att söka och ersätta text är `str_replace()` från `<algorithm>` biblioteket. Detta kan användas för att enkelt byta ut en viss sträng mot en annan i en given text.

```C++
// Byter ut alla förekomster av "solen" mot "månen"
#include <iostream>
#include <string>
#include <algorithm>

int main(){
    std::string sentence = "solen går upp när sommaren kommer";
    std::replace(sentence.begin(), sentence.end(), "solen", "månen");

    std::cout << sentence << std::endl;

    return 0;
}
```

I exemplet ovan används `replace()` för att byta ut alla förekomster av "solen" mot "månen" i en given text. Genom att använda `begin()` och `end()` funktionerna kan man ange att sökningen ska göras i hela strängen.

## Deep Dive

När man söker och ersätter text i en given kod eller fil finns det vissa faktorer man bör tänka på för att undvika oönskade resultat. En vanlig fallgrop är att sökningen och ersättningen sker på hela strängen, vilket kan leda till att andra delar av kod eller text också påverkas. Det är viktigt att noggrant ange vilka delar av strängen som ska sökas och eventuellt ersättas.

En annan viktig aspekt att tänka på är att vara noga med teckenkodning. Om man söker och ersätter text i en kod eller fil som har en annan teckenkodning än den standard som används i ens kod, kan det leda till oönskade resultat eller till och med göra att koden eller filen slutar fungera.

## Se även

- [C++ String