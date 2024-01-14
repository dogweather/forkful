---
title:    "C++: Skapa en tillfällig fil"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Varför du skulle vilja skapa en temporär fil i C++
Att skapa en temporär fil är ett vanligt och användbart verktyg inom programmering. Oavsett om du behöver spara temporär data, hantera säkerhetskopieringar eller hantera tillfälliga filer, är det här en effektiv lösning.

## Hur man skapar en temporär fil i C++
För att skapa en temporär fil i C++, behöver du först inkludera "fstream" biblioteket. Sedan kan du använda den inbyggda funktionen "tmpnam()" för att generera en unik filenamesning för din temporära fil. Här är ett exempel på kod som visar hur du skapar en temporär fil och skriver till den:

```C++
#include <iostream>
#include <fstream>

int main () {
  std::ofstream tempFile;
  char fileName[L_tmpnam];
  tmpnam(fileName); // genererar en unik filenamesning
  tempFile.open(fileName); // öppnar filen
  if(tempFile.is_open()) { // kollar om filen är öppen
    tempFile << "Det här är en temporär fil!"; // skriver till filen
    tempFile.close(); // stänger filen
    std::cout << "Temporär fil skapad med namnet " << fileName << std::endl;
  }
  return 0;
}
```
När du kör koden ovan kommer du att se ett meddelande som säger "Temporär fil skapad med namnet ..." följt av namnet på den temporära filen.

## Djupdykning i skapande av temporära filer
En temporär fil är en fil som skapas dynamiskt under programmets körning och raderas automatiskt när programmet avslutas. Detta gör att du kan hantera tillfälliga data utan att behöva oroa dig för att radera dem manuellt. När du använder funktionen "tmpnam()" skapas en fil med ett unikt namn baserat på systemets tillfälliga katalog. Detta namn kan sedan användas för att öppna filen och skriva till den.

## Se även
- [C++ ofstream](https://www.cplusplus.com/reference/fstream/ofstream/)
- [tmpnam() function in C++](https://www.geeksforgeeks.org/tmpnam-function-in-c/)
- [Creating temporary files in C++](https://www.tutorialspoint.com/creating-temporary-files-in-cplusplus)