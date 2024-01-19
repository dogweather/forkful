---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Vad och Varför?
Läsa en textfil i programmering innebär processen att hämta och tolka data från en fil på din dator. Programmerare gör detta för att hantera stora datamängder, eller för att spara och hämta information för framtida användning.

# Hur gör man:
I C++ kan vi använda inbyggda bibliotek som `fstream` för att läsa från en textfil. Här är ett enkelt exempel:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::string line;
    std::ifstream file("example.txt");  
    
    if (file.is_open()) {
        while (getline(file, line)) {
            std::cout << line << '\n';
        }
        file.close();
    } else {
        std::cout << "Unable to open file";
    }  

    return 0;
}
```
Denna kod öppnar en textfil som heter `example.txt`, läser varje rad tills den når filens slut och skriver ut varje rad till konsolen. Om filen inte kan öppnas skrivs "Unable to open file" ut på skärmen.

# Djup dykning
Historiskt sett, läsning av textfiler har varit en grundläggande operation i programmering sedan de tidigaste dagarna av datorer. Detta gjorde det möjligt för program att kommunicera med varandra och dela data.

Det finns också alternativa sätt att läsa en textfil i C++. Exempelvis, vi kan använda C standardbiblioteket `stdio.h` med `fscanf` eller `fgets` funktioner. Men `fstream` har fördelen av att vara mer 'C++ stil' och stöder exception hantering och RAII principer.

När det gäller implementation detaljer, `getline` funktionen läser rad för rad från filen. Funktionen `is_open` kontrollerar om filen var framgångsrikt öppnad innan vi försöker läsa från den.

# Se även
För mer information om att läsa och skriva till textfiler i C++, kolla in dessa källor:

- [cplusplus.com - File I/O](http://www.cplusplus.com/doc/tutorial/files/)
- [learncpp.com - Reading and writing to file](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)
- [stackoverflow.com - How to read a text file](https://stackoverflow.com/questions/7868936/read-file-line-by-line)