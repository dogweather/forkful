---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att få det nuvarande datumet är en process där programmet får information om aktuellt datum från operativsystemet. Programmerare gör detta för att spåra händelser, logga information eller hantera datumbaserade operationer. 

## Hur man gör:

Här är ett enkelt exempel på hur man får dagens datum i C-programmering. Den här koden använder `time.h` biblioteket för att få aktuellt datum.

```C
#include <time.h>
#include <stdio.h>

int main() {
   time_t nu;
   time(&nu);
   
   printf("Dagens datum: %s", ctime(&nu));
   
   return 0;
}
```

Kör denna kod, och utdatan kommer att vara något liknande:

```C
Dagens datum: Wed Sep 15 14:25:06 2021
```

## Djupdykning

Få det aktuella datumet är en grundläggande funktion i programmering som har använts sedan början av datorteori. C standardbiblioteket `time.h` innefattar funktioner att manipulera datum och tid.

Det finns alternativ till `time.h` biblioteket, som `sys/time.h` vilket ger mer precision. Beslutet vilket bibliotek att använda beror på de specifika kraven i ditt projekt.

Få det aktuella datumet i C sker genom funktionen `time()`, som returnerar systemtiden. Denna tid kan sedan konverteras till en mer läsbar form med `ctime()` funktionen.

## Se också

För mer läsning om tidsfunktioner i C, besök följande länkar:

1. C Library - `<time.h>`: https://www.tutorialspoint.com/c_standard_library/time_h.htm
2. Time and Date in C: https://www.geekhideout.com/cdate.shtml
3. `sys/time.h` and `time.h`: https://stackoverflow.com/questions/24104313/how-do-i-use-sys-time-h