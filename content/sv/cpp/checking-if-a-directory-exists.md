---
title:    "C++: Kontrollera om en katalog finns"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp finns är en viktig del av programmering eftersom det ofta är nödvändigt att hantera filer och mappar i ett program. Genom att kontrollera om en mapp existerar kan du utföra åtgärder baserade på det, till exempel skapa nya filer eller mappar, eller ta bort befintliga.

## Så här

För att kontrollera om en mapp existerar i C++ kan du använda funktionen `opendir()` från biblioteket `<dirent.h>`. Här är ett enkelt exempel på hur du kan använda det:

```C++
#include <iostream> 
#include <dirent.h> 
using namespace std; 
  
int main() 
{ 
    string mappnamn = "min_mapp"; 
      
    // Öppna mappen och spara en pekare till den 
    DIR* mapp = opendir(mappnamn.c_str()); 
      
    // Om pekaren är NULL så finns inte mappen 
    if(mapp == NULL)   
        cout << "Mappen existerar inte"; 
    else
    	cout << "Mappen existerar"; 
      
    // Stäng mappen 
    closedir(mapp); 
      
    return 0; 
} 
```

Om mappen `min_mapp` existerar kommer programmet att skriva ut "Mappen existerar", annars kommer det att skriva ut "Mappen existerar inte".

## Djupdykning

Om du vill ha mer kontroll över att kontrollera om en mapp existerar i C++ kan du använda flera olika metoder. En av dessa metoder är genom att använda funktionen `stat()` från biblioteket `<sys/stat.h>`. Den här funktionen kan ge dig mer detaljerad information om en fil eller mapp, till exempel om den är en vanlig fil eller en katalog. Här är ett exempel på hur du kan använda den:

```C++
#include <iostream> 
#include <sys/stat.h> 
using namespace std; 
  
int main() 
{ 
    string mappnamn = "min_mapp"; 
      
    // Skapa en struct för att lagra information om filen/mappen 
    struct stat buffer; 
      
    // Använd funktionen stat() för att få information om mappen 
    int kod = stat(mappnamn.c_str(), &buffer); 
      
    // Om kod är 0 så finns mappen 
    if(kod == 0)   
        cout << "Mappen existerar"; 
    else
    	cout << "Mappen existerar inte"; 
      
    return 0; 
} 
```

Även här kommer programmet att skriva ut om mappen existerar eller inte. Om mappen existerar kommer funktionen att returnera 0 och om den inte existerar kommer den att returnera ett annat värde.

## Se även

- [cppreference.com](https://en.cppreference.com/w/c/io)
- [GeeksforGeeks](https://www.geeksforgeeks.org/working-directory-c-3/)
- [C++ Standard Library](https://www.cplusplus.com/reference/cstdio/)