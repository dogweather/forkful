---
title:    "C: Omvandla en sträng till gemener"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför?
Att konvertera en sträng till små bokstäver kan vara användbart när man behöver jämföra strängar utan att behöva ta hänsyn till bokstavsstorlek. Det kan också vara till nytta i olika sorters textbehandlingsprogram eller för att enkelt formatera strängar på ett enhetligt sätt.

## Hur gör man?
För att konvertera en sträng till små bokstäver i C finns det flera olika metoder. En av de enklaste sätten är att använda standardfunktionen `tolower()` som finns i C-biblioteket <ctype.h>. Denna funktion tar en enskild bokstav som argument och returnerar en omvandlad version i små bokstäver.

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char str[] = "HEJ PÅ DIG";
    int i = 0;

    // Loopar genom varje tecken i strängen
    while (str[i]) {
    
        // Omvandlar tecknet till små bokstäver
        str[i] = tolower(str[i]);
        i++;
    }

    printf("%s\n", str); // Output: hej på dig

    return 0;
}
```

En annan metod är att använda sig av standardfunktionen `strlwr()` som finns i <string.h>. Denna funktion tar en hel sträng som argument och omvandlar alla bokstäver i strängen till små bokstäver.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "DAGEN IDAG";
    
    // Omvandlar hela strängen till små bokstäver
    strlwr(str);

    printf("%s\n", str); // Output: dagen idag

    return 0;
}
```

Observera att båda dessa metoder ändrar strängen permanent. Om man vill behålla den ursprungliga strängen kan man istället skapa en kopia och omvandla denna istället.

## Djupdykning
Bakom kulisserna använder dessa funktioner sig av ASCII-koderna för att utföra omvandlingen. Varje tecken har en unik ASCII-kod, och för bokstäverna A-Z finns det både en stor och en liten version av samma bokstav. Genom att manipulera ASCII-koderna kan man enkelt konvertera mellan olika bokstäver.

Det är också möjligt att skriva en egen funktion för att konvertera en sträng till små bokstäver. Ett sätt att göra detta är att loopa genom varje tecken och använda sig av matematiska operationer för att identifiera och omvandla bokstäverna.

## Se även
- [ASCII](https://sv.wikipedia.org/wiki/ASCII)
- [tolower()](https://www.programiz.com/c-programming/library-function/ctype.h/tolower)
- [strlwr()](https://www.programiz.com/c-programming/library-function/string.h/strlwr)