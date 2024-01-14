---
title:                "C++: Att skriva en textfil"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

Varför: Att skriva en textfil kan vara en nyttig och viktig del av programmering. Det gör det möjligt för användare att spara och lagra information på ett strukturerat sätt.

Hur man gör det: Det finns flera sätt att skriva en textfil i C ++, men den enklaste metoden är att använda funktionen "ofstream". Här är ett exempel på kod som visar hur man öppnar en fil, skriver in en sträng och sedan stänger filen:

```C++
#include <iostream>
#include <fstream>  // inkludera filhanteringsbiblioteket

using namespace std;

int main()
{
    // skapa ett ofstream-objekt för att hantera filen
    // filnamnet "mitt_test.txt" kan bytas ut mot önskat filnamn
    ofstream min_fil("mitt_test.txt");

    // skriv in en sträng i filen
    minFil << "Hej, det här är en textsträng som sparas i filen.";

    // kom ihåg att stänga filen när all data har skrivits
    minFil.close();
    
    return 0;
}
```

Om du vill kontrollera om filen skapades och skrevs korrekt kan du använda "ifstream" -funktionen för att öppna och läsa filen. Här är ett exempel på kod som läser filen och skriver ut innehållet på skärmen:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main()
{
    ifstream min_fil("mitt_test.txt");

    // läs varje rad i filen tills det inte finns några fler att läsa
    while (getline(min_fil, rad))
    {
        cout << rad << '\n';
    }

    // stäng filen när all data har lästs
    min_fil.close();

    return 0;
}
```

Ett annat sätt att öppna och skriva i en fil är att använda funktionen "fopen" och "fprintf". Här är ett exempel på kod som visar hur man skapar och skriver i en fil:

```C++
#include <stdio.h>

int main()
{
    // skapa ett FILE-objekt för att hantera filen
    // "mitt_test2.txt" kan bytas ut mot önskat filnamn
    FILE *min_fil = fopen("mitt_test2.txt", "w");

    // skriv in en sträng i filen
    fprintf(min_fil, "Hej, det här är en textsträng som skrivs i filen.");

    // kom ihåg att stänga filen när all data har skrivits
    fclose(min_fil);

    return 0;
}
```

Djupdykning: När du skriver en textfil är det viktigt att vara medveten om filformatet. Ett vanligt format är "textfil" eller "plain text", där varje tecken sparas exakt som det är utan någon extra formatering eller kodning. Detta är den typ av fil som genereras när du använder funktionerna "ofstream" eller "fprintf". En annan typ av fil är "binär" eller "binär data", där information kan sparas effektivare genom att koda den i ett annat format, till exempel "byte" eller "hexadecimal". Detta är särskilt användbart när du har en stor mängd data som ska sparas. Du kan lära dig mer om de olika filformaten och deras användning i din specifika programmeringsmiljö.

Se även: Här är några resurser som kan hjälpa dig att lära dig mer om att skriva textfiler i C ++:

- [cplusplus.com - File I/O in C++](http://www.cplusplus.com/doc/tutorial/files/)
- [GeeksforGeeks - File handling in C++](https://www.geeksforgeeks.org/file-handling-c-classes/)
- [Programiz - C++ File Handling](https://www.programiz.com/cpp-programming/file-io)