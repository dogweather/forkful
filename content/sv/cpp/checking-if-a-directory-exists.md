---
title:                "Kontrollera om en mapp existerar"
html_title:           "C++: Kontrollera om en mapp existerar"
simple_title:         "Kontrollera om en mapp existerar"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Varför
Att kunna kontrollera om en mapp finns är en viktig del av programmering. Detta gör det möjligt att hantera olika scenarier och undvika problem som kan uppstå om en mapp inte finns.

# Så här gör du
Du kan enkelt kontrollera om en mapp finns genom att använda funktionen `opendir()` i C++. Funktionen tar in mappens sökväg som argument och returnerar en pekare till mappen om den finns, annars returneras `NULL`.

```C++
#include <iostream>
#include <dirent.h>

int main(){

    // Ange mappens sökväg
    const char* path = "/Users/user/Documents/ExampleFolder";

    // Kontrollera om mappen finns
    DIR* dir = opendir(path);

    // Om mappen finns returneras en pekare
    if (dir){
        std::cout << "Mappen finns!" << std::endl;
        closedir(dir); // Stäng pekaren
    }
    // Annars returneras NULL
    else{
        std::cout << "Mappen finns inte!" << std::endl;
    }

    return 0;
}
```

**Output:**
```
Mappen finns inte!
```

Om du vill kunna göra fler åtgärder beroende på om mappen finns eller inte, kan du använda styrkoden `if/else` som visas i exemplet ovan. Om mappen finns kan du till exempel skapa en ny mapp med samma namn eller läsa innehållet i mappen.

# Djupdykning
För att kunna kontrollera om en mapp finns måste du använda funktionen `opendir()` från C:s standardasbibliotek. Denna funktion öppnar en filström till den angivna mappen och returnerar en pekare till den. Om mappen inte finns returnerar funktionen `NULL`.

Det är viktigt att nämna att funktionen `opendir()` bara kontrollerar om det finns någon filström till mappen, inte om det verkligen är en mapp eller filer i den. Om du behöver kontrollera om det finns filer i mappen måste du loopa igenom innehållet med hjälp av funktionen `readdir()`.

# Se även
* [Dokumentation för opendir()](https://www.cplusplus.com/reference/cstdio/opendir/)
* [Hantering av mappar i C++](https://programmerare.se/hantering-av-mappar-i-c/)