---
title:                "Loggning"
date:                  2024-01-26T01:00:50.848826-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggning"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/logging.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att logga är i grund och botten att skriva ner vad ditt program gör, vanligen genom att skicka ut meddelanden till en fil eller terminal. Programmerare gör det för att hålla koll på händelser, felsöka problem, och för att ha en revisionsspårning som berättar historien om en applikations funktion över tid.

## Hur man gör:
Låt oss börja med några grunder. C har inget inbyggt ramverk för loggning, men du kan skapa något enkelt med `stdio.h`. Så här gör du:

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t now;
    time(&now);
    char *date = ctime(&now);
    date[strlen(date) - 1] = '\0'; // Ta bort radslutet i slutet av ctime()'s resultat
    printf("[%s] %s\n", date, message);
}

int main() {
    logMessage("Applikationen har startat.");
    // ... din kod kommer här ...
    logMessage("Applikationen utför något viktigt.");
    // ... din kod fortsätter ...
    logMessage("Applikationen har avslutats.");
    return 0;
}
```

Ett exempel på utskrift kan se ut så här:

```
[Tis Mar 9 12:00:01 2023] Applikationen har startat.
[Tis Mar 9 12:00:02 2023] Applikationen utför något viktigt.
[Tis Mar 9 12:00:03 2023] Applikationen har avslutats.
```

Självklart skulle du i verkliga världen förmodligen vilja skriva till en fil istället för terminalen, hantera olika loggnivåer och kanske använda ett fördefinierat bibliotek.

## Fördjupning
Att logga i C har en egen charm – det är lika lågnivå som resten av språket. Historiskt sätt utfördes loggning med `fprintf` tillsammans med `stderr` eller en filpekare. När program blev mer komplexa, blev också behoven för loggning det, vilket ledde till utvecklingen av bibliotek som `syslog` på Unix-system, som kunde hantera loggning från flera källor med olika viktnivåer.

I det moderna landskapet finns det gott om loggbibliotek för C, såsom `zlog`, `log4c` och `glog`, som erbjuder en rik uppsättning funktioner inklusive loggrotation, strukturerad loggning och multitrådad loggning. Dessa lösningar tillåter finjusterad kontroll över loggningens verbositet, destinationer och format.

När du implementerar ett loggsystem behöver detaljer som tidsstämpelformat, hantering av loggfiler och prestanda övervägas. Tidsstämpling av loggar är avgörande för att korrelera händelser, medan loggrotation säkerställer att loggfiler inte förbrukar för mycket diskutrymme. Själva loggningshandlingen bör också vara snabb och icke-blockerande för applikationens huvudflöde för att förhindra att loggningen blir en flaskhals.

## Se även
För att fördjupa dig ytterligare i loggbibliotek och praxis i C, kolla in dessa resurser:

- GNU `syslog` manual: https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog`: Ett högt konfigurerbart loggbibliotek för C - https://github.com/HardySimpson/zlog
- `log4c`: Ett loggningsramverk för C modellerat efter Log4j - http://log4c.sourceforge.net/
- `glog`: Googles programnivå loggbibliotek - https://github.com/google/glog
