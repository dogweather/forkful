---
title:    "C++: Jämförande av två datum"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum kan vara en användbar teknik för att hantera tidsstyrda processer, såsom schemaläggning eller beräkning av tidsintervaller. Att ha kunskap om hur man jämför datum i C++ kan därför hjälpa till att skapa effektivare och mer exakta applikationer.

## Hur Man Gör
För att jämföra två datum i C++ behöver man använda sig av klassen `std::chrono::system_clock` och dess `now()` och `time_since_epoch()` funktioner. Nedan följer ett enkelt exempel:

```C++
#include <iostream>
#include <chrono>

int main() {
    // Skapar två tidpunkter och lagrar skillnaden i variabeln diff
    // Tiden anges i sekunder sedan 1 januari 1970
    auto time1 = std::chrono::system_clock::now();
    auto time2 = std::chrono::system_clock::now();
    std::chrono::duration<double> diff = time2 - time1;

    // Utöver skillnaden i sekunder kan man även jämföra tidpunkterna 
    // med hjälp av operatorerna >, <, >=, <=, == och !=
    if (time2 > time1) {
        std::cout << "time2 är senare än time1\n";
    }

    // Output: time2 är senare än time1
    // För att få ut skillnaden i sekunder kan man använda följande uttryck:
    std::cout << "Skillnaden mellan time1 och time2 i sekunder är: "
              << diff.count() << " s\n";
    
    // Output: Skillnaden mellan time1 och time2 i sekunder är: 0 s

    return 0;
}
```
Outputen kommer att visa att `time2` är senare än `time1` och att skillnaden mellan dem är 0 sekunder.

## Djupdykning
Det finns flera aspekter att ta hänsyn till när man jämför datum i C++. En av dessa är vilken precision man behöver ha för sina tidsmätningar. I exemplet ovan användes `std::chrono::duration`. Med denna klass kan man specifiera precisionen på tidsenheterna, till exempel sekunder eller millisekunder.

En annan viktig aspekt är att använda rätt tidszon för att undvika eventuella tidszonkonflikter. Detta kan göras genom att använda klassen `std::chrono::time_zone` och dess `locate_zone()` funktion.

När man arbetar med datum och tider är det också viktigt att ta hänsyn till eventuella skottår eller skillnader mellan kalendrar, vilket kan påverka resultatet av jämförelser. Det finns bibliotek som erbjuder API:er för att hantera dessa komplexiteter, såsom `boost:date_time` eller `Qt Core`.

## Se Även
- [C++ Standardbiblioteket: Datum och Tid](https://www.cplusplus.com/reference/chrono/)
- [Edinburgh Napier University: C++ Jämföra Datum](https://web.archive.org/web/20170521070902/https://computing.napier.ac.uk/~mhuber/cu_cpp/course_notes/part3/node157.html)