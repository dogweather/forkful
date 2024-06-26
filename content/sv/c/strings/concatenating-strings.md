---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:53.673816-07:00
description: "Hur man g\xF6r: I C \xE4r str\xE4ngar arrayer av tecken som avslutas\
  \ med ett null-tecken (`\\0`). Till skillnad fr\xE5n i h\xF6gre programmeringsspr\xE5\
  k, tillhandah\xE5ller C\u2026"
lastmod: '2024-03-13T22:44:38.373534-06:00'
model: gpt-4-0125-preview
summary: "I C \xE4r str\xE4ngar arrayer av tecken som avslutas med ett null-tecken\
  \ (`\\0`)."
title: "Sammanfoga str\xE4ngar"
weight: 3
---

## Hur man gör:
I C är strängar arrayer av tecken som avslutas med ett null-tecken (`\0`). Till skillnad från i högre programmeringsspråk, tillhandahåller C inte en inbyggd funktion för strängkonkatenering. Istället använder du funktionerna `strcat()` eller `strncat()` från biblioteket `<string.h>`.

Här är ett enkelt exempel som använder `strcat()`:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hej, ";
    char source[] = "Världen!";

    strcat(destination, source);

    printf("%s\n", destination);  // Utdata: Hej, Världen!
    return 0;
}
```

Funktionen `strcat()` tar två argument: destinationssträngen (som måste ha tillräckligt med utrymme för att hålla det sammanfogade resultatet) och källsträngen. Den lägger sedan till källsträngen till destinationssträngen.

För mer kontroll över antalet tecken som konkateneras, är `strncat()` säkrare att använda:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hej, ";
    char source[] = "Världen!";
    int num = 3; // Antalet tecken att lägga till

    strncat(destination, source, num);

    printf("%s\n", destination);  // Utdata: Hej, Vär
    return 0;
}
```

Detta begränsar konkateneringen till de första `num` tecknen i källsträngen, vilket hjälper till att förhindra buffertöverskridningar.

## Fördjupning
Funktionerna `strcat()` och `strncat()` har varit en del av C:s standardbibliotek sedan dess början, vilket återspeglar språkets lågnivåkaraktär som kräver manuell hantering av strängar och minne. Till skillnad från många moderna programmeringsspråk som behandlar strängar som förstaklassens objekt med inbyggda konkateneringsoperatorer (som `+` eller `.concat()`), kräver C:s tillvägagångssätt en djupare förståelse för pekare, minnesallokering och potentiella fallgropar som buffertöverskridningar.

Även om `strcat()` och `strncat()` är brett använda, kritiseras de ofta för deras potential att skapa säkerhetsproblem om de inte används försiktigt. Buffertöverskridningar, där data överskrider det tilldelade minnet, kan leda till krascher eller utnyttjas för godtycklig kodexekvering. Som ett resultat vänder sig programmerare alltmer till säkrare alternativ, såsom `snprintf()`, som ger mer förutsägbart beteende genom att begränsa antalet tecken som skrivs till destinationssträngen baserat på dess storlek:

```c
char destination[50] = "Hej, ";
char source[] = "Världen!";
snprintf(destination + strlen(destination), sizeof(destination) - strlen(destination), "%s", source);
```

Denna metod är mer långdragen men betydligt säkrare, vilket markerar en förflyttning i C-programmeringspraxis mot att prioritera säkerhet och robusthet framför korthet.

Trots dessa utmaningar är strängkonkatenering i C en grundläggande färdighet, avgörande för effektiv programmering i språket. Att förstå dess nyanser och associerade risker är nyckeln till att bemästra C-programmering.
