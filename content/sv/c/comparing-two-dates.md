---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att jämföra två datum handlar om att se vilket datum är tidigare, senare eller om de är samma dag. Programmerare gör detta för att få inblick i händelsernas kronologi, tidsintervall och för att utföra andra datumrelaterade beräkningar.

## Så här gör du:

Vi kommer att använda `tm` strukturen och `mktime` funktionen för att jämföra datum. Här är en snabb exempelkod:

```C
#include <time.h>
#include <stdio.h>

int main() {
    // Skapa två olika 'struct tm' objekt.
    struct tm a = { .tm_year=120, .tm_mon=5, .tm_mday=30 };
    struct tm b = { .tm_year=100, .tm_mon=2, .tm_mday=15 };

    // Jämföra de två datumen.
    double seconds = difftime(mktime(&a), mktime(&b));
    
    if(seconds < 0) {
        printf("a är tidigare än b");
    } else if (seconds > 0) {
        printf("a är senare än b");
    } else {
        printf("a och b är samma dag");
    }

    return 0;
}
```

Om du kör detta program, kommer utdata att vara "a är senare än b".

## Djup Dopp

Att jämföra datum är inte nytt, och olika tillvägagångssätt har använts genom tiden. Den metoden vi visade använder C's inbyggda `tm` struktur och `mktime` funktion, ingår i `<time.h>` biblioteket som varit en del av C standarden sedan C89.

Alternativa metoder kan inkludera att manuellt jämföra år, månader och dagar, men att använda inbyggda funktioner som `mktime` kan ofta ge enklare och mer korrekta resultat, eftersom de tar hänsyn till saker som skottår och olika månadslängder.

## Se Även

För mer information om `tm` struktur och `mktime` funktion, besök dessa länkar:
- [tm struct on cplusplus.com](http://www.cplusplus.com/reference/ctime/tm/)
- [mktime function on cplusplus.com](http://www.cplusplus.com/reference/ctime/mktime/)