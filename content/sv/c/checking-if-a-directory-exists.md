---
title:    "C: Kontrollera om en mapp finns"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar är en viktig del av programmering i C. Detta är särskilt användbart när du vill tillåta användaren att läsa eller skriva till en befintlig mapp eller fil.

## Hur man gör

Ett enkelt sätt att kontrollera om en mapp existerar är genom att använda funktionen "opendir" tillsammans med "if-statement". Här är ett exempel:

```C
#include <stdio.h> 
#include <dirent.h> 

int main() 
{ 
  // Öppna mappen 
  DIR *dir = opendir("/Users/username/Documents"); 
  
  // Kontrollera om mappen existerar 
  if (dir == NULL) { 
    printf("Mappen finns inte"); 
  } 
  else { 
    printf("Mappen finns"); 
    closedir(dir); 
  } 
  
  return 0; 
} 
```

Om mappen existerar kommer utskrift att vara "Mappen finns", annars kommer utskrift att vara "Mappen finns inte". Detta är ett enkelt sätt att kontrollera en mapp i C.

## Djupdykning

Det finns flera andra sätt att kontrollera om en mapp existerar i C. Ett annat sätt är att använda funktionen "stat" tillsammans med "if-statement". Här är ett exempel:

```C
#include <stdio.h> 
#include <sys/stat.h> 

int main() 
{ 
  // Skapa en struktur för att hålla information om filen 
  struct stat st = {0}; 
  
  // Kontrollera filens information 
  if (stat("/Users/username/Documents", &st) == -1) { 
    printf("Mappen finns inte"); 
  } 
  else { 
    printf("Mappen finns"); 
  } 
  
  return 0; 
} 
```

Funktionen "stat" returnerar information om en fil eller mapp, så om mappen existerar kommer utskrift att vara "Mappen finns", annars kommer utskrift att vara "Mappen finns inte".

Det är också möjligt att skapa en egen funktion för att kontrollera om en mapp existerar. Detta ger dig mer kontroll och flexibilitet när det gäller att hantera fel och skapa anpassade meddelanden.

## Se även

- [Dokumentation om opendir()](https://www.geeksforgeeks.org/opendir-library-function-in-c-language/)
- [Dokumentation om stat()](https://www.geeksforgeeks.org/stat-function-in-c/#:~:text=stat()%20is%20a%20system%20call%20used%20to%20get%20the,attributes%20of%20a%20file%20located%20in%20a%20Linux%20filesystem.)