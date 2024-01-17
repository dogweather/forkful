---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "C: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Beräkna ett datum i framtiden eller det förflutna handlar om att förutsäga vilket datum som skulle vara efter eller före ett visst tidsintervall från det aktuella datumet. Programmerare använder det för att utföra olika uppgifter som involverar datum, som till exempel att schemalägga uppgifter eller beräkna olika tidsintervall.

# Så här gör du:

### Beräkna ett datum i framtiden:

```C
#include <stdio.h> 
#include <time.h> 
  
int main () 
{ 
   struct tm * datum; 
   time_t aktuellt_datum; 
  
   //Hämta det aktuella datumet
   time(&aktuellt_datum); 
  
   //Lägg till 10 dagar till aktuellt datum
   aktuellt_datum += (10 * 86400); 
  
   //Konvertera tillbaka till tm-struktur 
   datum = localtime(&aktuellt_datum); 
  
   //Skriv ut det nya datumet
   printf("10 dagar från nu är det: %d-%d-%d\n", datum->tm_mday, datum->tm_mon+1, datum->tm_year+1900);
  
   return 0; 
} 
```

Output:

10 dagar från nu är det: [datum]

### Beräkna ett datum i förflutnan:

```C
#include <stdio.h> 
#include <time.h> 
  
int main () 
{ 
   struct tm * datum; 
   time_t aktuellt_datum; 
  
   //Hämta det aktuella datumet
   time(&aktuellt_datum); 
  
   //Ta bort 10 dagar från aktuellt datum
   aktuellt_datum -= (10 * 86400); 
  
   //Konvertera tillbaka till tm-struktur 
   datum = localtime(&aktuellt_datum); 
  
   //Skriv ut det nya datumet
   printf("10 dagar sedan var det: %d-%d-%d\n", datum->tm_mday, datum->tm_mon+1, datum->tm_year+1900);
  
   return 0; 
} 
```

Output:

10 dagar sedan var det: [datum]

# Deep Dive:

Att kunna beräkna datum i framtiden eller förflutnan är en viktig del av programmering eftersom många uppgifter involverar datum och tidsintervall. Att använda C för att beräkna datum ger en tillförlitlig och enkel lösning. Det finns också andra bibliotek som kan användas för att hantera datum, som till exempel "ctime" och "chrono" i C++, men de är mer komplexa att använda.

# Se också:

- Länk till "ctime" biblioteket: [insert link]
- Länk till "chrono" biblioteket: [insert link]