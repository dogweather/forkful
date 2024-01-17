---
title:                "Att få den nuvarande datumet."
html_title:           "C: Att få den nuvarande datumet."
simple_title:         "Att få den nuvarande datumet."
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att få den aktuella datumet är en mycket vanlig uppgift för programmerare. Det hjälper till att hålla koll på när ett program eller en applikation är i drift och om det behöver uppdateras eller inte.

## Hur man gör:

För att få den aktuella datumet i C, används funktionen `time()` tillsammans med `ctime()` för att konvertera tidsinformationen till ett läsbar format. Se kodexemplet nedan:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Hämtar den aktuella datumet
  time_t t = time(NULL);

  // Konverterar till ett läsbart format
  char *aktuellt_datum = ctime(&t);

  // Skriver ut den aktuella datumet
  printf("Aktuellt datum: %s", aktuellt_datum);

  return 0;
}
```

### Utskrift:
```
Aktuellt datum: Fri Feb 05 12:34:17 2021
```

## Djupdykning:

Att få den aktuella datumet har varit en viktig uppgift för programmerare sedan begynnelsen av datorer. Innan standardbiblioteket `time.h` introducerades, var det mycket mer komplicerat att få den aktuella datumet. Alternativen idag inkluderar användning av API:er från olika operativsystem eller användning av färdiga bibliotek som `boost::date_time` för C++. 

När funktionen `time()` anropas, returneras antalet sekunder sedan UNIX-epoken (1 januari 1970 00:00:00 UTC). Sedan konverteras detta nummer till en läsbar sträng med funktionen `ctime()`.

## Se även:

- [C time library](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [boost::date_time](https://www.boost.org/doc/libs/1_72_0/doc/html/date_time.html)
- [Alternative methods for getting current date in C](https://www.geeksforgeeks.org/alternative-ways-get-current-date-java/)