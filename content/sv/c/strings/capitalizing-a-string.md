---
title:                "Gör om en sträng till versaler"
aliases:
- sv/c/capitalizing-a-string.md
date:                  2024-02-03T17:52:58.190284-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att göra en sträng versal i C innebär att konvertera det första tecknet i varje ord i en given sträng till versal om det är en gemen bokstav. Programmerare utför ofta denna operation för att standardisera användarinput för sökningar, sorteringsoperationer eller visningssyften, vilket säkerställer konsekvens och läsbarhet i textdata.

## Hur man gör:

Att göra en sträng versal i C kräver en grundläggande förståelse för teckenmanipulation och strängtraversering. Eftersom C inte har en inbyggd funktion för detta, kommer du vanligtvis att kontrollera varje tecken, och justera dess fall vid behov. Nedan är en enkel implementation:

```c
#include <stdio.h>
#include <ctype.h> // För islower och toupper funktioner

void capitalizeString(char *str) {
    if (str == NULL) return; // Säkerhetskontroll
    
    int capNext = 1; // Flagga för att ange om nästa bokstav ska göras versal
    for (int i = 0; str[i] != '\0'; i++) {
        if (capNext && islower(str[i])) {
            str[i] = toupper(str[i]); // Gör tecken versalt
            capNext = 0; // Återställ flagga
        } else if (str[i] == ' ') {
            capNext = 1; // Nästa tecken bör göras versalt
        }
    }
}

int main() {
    char exampleString[] = "hello world. programming in c!";
    capitalizeString(exampleString);
    printf("Capitalized string: %s\n", exampleString);
    return 0;
}
```

Exempelutdata:
```
Capitalized string: Hello World. Programming In C!
```

Detta program traverserar strängen `exampleString`, kontrollerar varje tecken om det ska göras versalt. Funktionen `islower` kontrollerar om ett tecken är en gemen bokstav, medan `toupper` konverterar det till versalt. Flaggen `capNext` bestämmer om nästa bokstav som stöts på bör konverteras, den ställs in efter varje mellanslag (' ') som hittas, och inledningsvis för att göra strängens första tecken versalt.

## Fördjupning

Tekniken som demonstreras är enkel men brister i effektivitet för mycket stora strängar eller när den utförs upprepade gånger i prestandakritiska applikationer. I historiskt och genomförandekontext involverar strängmanipulation i C, inklusive kapitalisering, ofta direkt buffertmanipulation, vilket återspeglar Cs lågnivåansats och ger programmeraren full kontroll över minne och prestandaavvägningar.

Det finns alternativa, mer sofistikerade metoder för att göra strängar versala, särskilt när man tar hänsyn till lokaler och Unicode-tecken, där kapitaliseringsregler kan skilja sig avsevärt från det enkla ASCII-scenariot. Bibliotek som ICU (International Components for Unicode) erbjuder robusta lösningar för dessa fall men introducerar beroenden och overhead som kanske inte är nödvändiga för alla applikationer.

Vidare, medan exemplet som tillhandahålls använder C-standardbiblioteksfunktionerna `islower` och `toupper`, som är del av `<ctype.h>`, är det viktigt att förstå att dessa fungerar inom ASCII-omfånget. För applikationer som kräver bearbetning av tecken bortom ASCII, såsom hantering av accenterade tecken i europeiska språk, kommer ytterligare logik eller tredjepartsbibliotek att vara nödvändigt för att exakt utföra kapitalisering.

Sammanfattningsvis, även om metoden som beskrivs är lämplig för många applikationer, är det avgörande att förstå dess begränsningar och de alternativ som finns tillgängliga för att utveckla robust, internationaliserad programvara i C.
