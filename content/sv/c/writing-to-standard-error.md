---
title:                "Skriva till standardfel"
date:                  2024-01-19
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standardfel (stderr) innebär att skicka felmeddelanden och diagnostik till en separat utström än normal utdata (stdout). Programmerare gör detta för att separera vanlig data från felmeddelanden, vilket underlättar felsökning och loggning.

## Hur man gör:
```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Ett fel inträffade!\n");
    return 0;
}
```
Output:
```
Ett fel inträffade!
```

## Fördjupning
Historiskt sett separerades stderr från stdout för att tillåta omdirigering av utdata och felmeddelanden till olika destinationer i terminalen. Alternativ inkluderar att logga till filer eller överföra data via rörledningar (pipes) i Unix-baserade system. Implementationsmässigt hanteras stderr av filbeskrivare 2 i Unix-system, medan stdout har filbeskrivare 1.

## Se även
- [GNU C Library - Standard Streams](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- [C Error Handling (tutorialspoint)](https://www.tutorialspoint.com/cprogramming/c_error_handling.htm)
