---
title:                "C++: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

##Varför

Att skapa en textfil är en grundläggande men viktig färdighet för alla som lär sig programmera i C++. Textfiler låter dig lagra och hantera data på ett enkelt och effektivt sätt. Det är ett viktigt steg för att förstå hur filer fungerar och för att kunna skriva mer komplexa program.

##Hur Man Gör

Först och främst behöver du inkludera biblioteket `fstream` för att kunna arbeta med filer. Sedan behöver du öppna en fil med hjälp av `ofstream` och ange vilken fil du vill arbeta med, t.ex. `myFile.txt`. Om filen inte finns kommer den att skapas automatiskt. Här är ett exempel på kod som skapar en textfil och skriver en rad med text i den:

```C++
#include <fstream>

using namespace std;

int main()
{
  ofstream myFile("myFile.txt");
  myFile << "Detta är en text i min fil.";
  myFile.close();
  return 0;
}
```

Notera att vi använder `ofstream` för att öppna filen i skrivläge och `myFile.close()` för att stänga filen igen efter att vi är klara med den.

Om du redan har en befintlig textfil som du vill lägga till mer text i, kan du använda `ofstream` tillsammans med `ios::app` för att lägga till texten i slutet av filen istället för att skriva över den befintliga texten:

```C++
ofstream myFile("myFile.txt", ios::app);
myFile << "Detta är den nya raden.";
```

Du kan även läsa in text från en befintlig fil med hjälp av `ifstream` och `getline()` funktionen. Här är ett exempel på kod som läser in text från en fil och skriver ut den till konsolen:

```C++
#include <fstream>
#include <iostream>

using namespace std;

int main()
{
  ifstream myFile("myFile.txt");
  string text;

  while (getline(myFile, text)) {
    cout << text << endl;
  }

  myFile.close();
  return 0;
}
```

##Djupdykning

Det finns många andra användbara funktioner och metoder för att arbeta med textfiler, som att läsa och skriva specifika delar av filen och hantera fel som kan uppstå under arbetet med filen. Det är viktigt att förstå hur dessa funktioner fungerar och använda dem på ett korrekt sätt för att undvika problem med dina filer.

Se även

- [Tutorialspoint - C++ File Input/Output](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [cplusplus.com - File streams](http://www.cplusplus.com/doc/tutorial/files/)
- [GeeksforGeeks - File Handling in C++](https://www.geeksforgeeks.org/file-handling-c-classes/)