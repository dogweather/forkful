---
title:                "C++: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Varför

Att söka och ersätta text är en vanlig uppgift inom programmering som kan spara mycket tid och ansträngning. Genom att använda en enkel sök-och-ersättningsfunktion kan du snabbt ändra flera förekomster av en specifik textsträng i ett dokument eller en kodbas. Detta är särskilt användbart när du behöver uppdatera en kodbas med nya variablenamn eller gör större förändringar i ett projekt.

##Så här

För att söka och ersätta en textsträng i C++ kan du använda strängfunktionen `find()` i kombination med `replace()`-funktionen. Här är ett enkelt exempel som söker efter alla förekomster av "hej" och ersätter dem med "hello" i en sträng:

```C++
string myString = "hej och hej då";
myString.replace(myString.find("hej"), 3, "hello");
```

Detta exempel kommer att ändra "hej" till "hello" på både första och andra förekomsten i strängen `myString`. Genom att använda `replace()`-funktionen tillsammans med `find()` kan du snabbt och effektivt ändra flera förekomster av en viss textsträng.

För att söka och ersätta i en hel fil använder du `ifstream` och `ofstream` för att öppna och skriva till filen. Här är ett exempel på hur detta kan göras:

```C++
ifstream inputFile("minFil.txt");
ofstream outputFile("ersattMinFil.txt");

string line;
while (getline(inputFile, line)) {
    line.replace(line.find("hej"), 3, "hello");
    outputFile << line << endl;
}
```

I detta exempel öppnas en fil för läsning och en annan fil skapas för att skriva det ändrade innehållet till. Sedan sätts en kommandorad som läser in varje rad i filen och ersätter alla förekomster av "hej" med "hello" med `replace()`-funktionen. Slutligen skrivs den ändrade raden till den nya filen med hjälp av `ofsstream`.

##Djupdykning

Även om söka och ersätta i C++ kan verka enkelt finns det vissa fall där det kan bli mer komplicerat. Till exempel om du vill söka och ersätta en textsträng som finns inuti citattecken kan det bli mer krångligt. Detta beror på att `find()`-funktionen söker efter en hel sträng, inte en del av en sträng. För att lösa detta problem kan du använda `substr()`-funktionen för att dela upp strängen i mindre delar och sedan använda `find()` för att söka och ersätta inuti dessa delsträngar.

Du kan också använda `regex` för att söka och ersätta text med hjälp av reguljära uttryck. Detta ger en mer flexibel sökning och något som kan vara användbart för mer komplexa uppgifter. Men det är viktigt att komma ihåg att `regex` kan vara resurskrävande och bör användas med försiktighet i större projekt.

##Se också

- [C++ String Class Reference](https://www.cplusplus.com/reference/string/string/)
- [C++ ifstream Class Reference](https://www.cplusplus.com/reference/fstream/ifstream/)
- [C++ ofstream Class Reference](https://www.cplusplus.com/reference/fstream/ofstream/)