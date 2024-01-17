---
title:                "Skapa en temporär fil"
html_title:           "C++: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skapande av en tillfällig fil i ett program innebär att skapa en fil som enbart behövs tillfälligt under körningstiden. Det kan vara användbart för att temporärt lagra data eller för att hantera filöverföringar. Programmers använder detta för att effektivt hantera datahantering och för att undvika permanent filskrivning i programmet.

## Hur går man tillväga:
```
#include <iostream>
#include <fstream>

using namespace std;

int main() {
  // Skapar en temporär fil som heter "temp.txt" 
  ofstream tempfile("temp.txt");

  // Skriver in text i filen
  tempfile << "Det här är en temporär fil\n";

  // Stänger filen
  tempfile.close();

  // Öppnar filen för läsning
  ifstream readfile("temp.txt");

  // Kollar om filen är öppen
  if (readfile.is_open()) {
    // Skriver ut innehållet av filen
    cout << "Filen innehåller: \n";
    string line;
    while (getline(readfile, line)) {
      cout << line << '\n';
    }
    // Stänger filen
    readfile.close();
  } else {
    // Om filen inte kunde öppnas
    cout << "Kunde inte öppna filen\n";
  }
  // Tar bort filen när den inte längre behövs
  remove("temp.txt");
  return 0;
}
```

Output:
```
Filen innehåller:
Det här är en temporär fil
```

## Djupdykning:
Skapande av tillfälliga filer är en vanlig praktik som används både inom operativsystem och inom programutveckling. Det används ofta för att skapa temporära backup-filer eller loggfiler som senare kan raderas för att spara utrymme. Istället för att konstant skriva till huvudfilen, kan en tillfällig fil skapas och sedan överföras eller kombineras med huvudfilen vid behov. Alternativen till att använda temporära filer inkluderar att skapa temporära variabler i minnet eller att använda en databas för att hantera data.

Implementationen av tillfälliga filer kan variera beroende på operativsystem och programmeringsspråk. I C++ görs detta genom användning av filestream-klassen för att skapa och skriva till filen, och sedan stänger och raderar filen när den inte längre behövs.

## Se även:
- [C++ filestream-dokumentation](https://www.cplusplus.com/reference/fstream/)
- [Umfangreiches STDIO.h Tutorial mit ASCII Tabellen](https://repl.it/talk/learn/ASCII-Tutorial-Part-1-Getting-Started-with-C/6832)