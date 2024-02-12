---
title:                "Hantera fel"
aliases:
- sv/c/handling-errors.md
date:                  2024-02-03T17:58:14.552011-07:00
model:                 gpt-4-0125-preview
simple_title:         "Hantera fel"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/handling-errors.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att hantera fel i C innebär att upptäcka och bemöta onormala förhållanden som uppstår under programkörning. Programmerare gör detta för att förhindra buggar, krascher och oförutsägbart beteende, vilket säkerställer att programvaran fungerar pålitligt och effektivt under olika scenarier.

## Hur man gör:

C har inte inbyggt stöd för undantag som vissa andra språk har. Istället förlitar det sig på några konventionella strategier för felhantering, såsom att returnera speciella värden från funktioner och att ställa in globala variabler som `errno`.

**Returnera Speciella Värden**

Funktioner kan indikera fel genom att returnera ett specifikt värde som är osannolikt att vara ett giltigt resultat. Här är ett exempel med heltal:

```c
#include <stdio.h>

int inverse(int number, double *result) {
    if (number == 0) {
        return -1; // Felaktigt fall
    } else {
        *result = 1.0 / number;
        return 0; // Framgång
    }
}

int main() {
    double result;
    if (inverse(0, &result) < 0) {
        printf("Fel: Division med noll.\n");
    } else {
        printf("Inversen är: %f\n", result);
    }
    
    return 0;
}
```

**Utskrift:**
```
Fel: Division med noll.
```

**Kontrollera `errno`**

För biblioteksfunktioner, särskilt de som interagerar med systemet eller operativsystemet (som fil-I/O), ställs `errno` in när ett fel uppstår. För att använda det, inkludera `errno.h` och kontrollera `errno` efter en misstänkt misslyckande:

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("nonexistent.txt", "r");
    if (file == NULL) {
        printf("Fel vid filöppning: %s\n", strerror(errno));
    }
    
    return 0;
}
```

**Utskrift:**
```
Fel vid filöppning: Filen eller katalogen finns inte
```

## Djupdykning

Historiskt sett har C-programmeringsspråkets minimalistiska design uteslutit en inbyggd mekanism för undantagshantering, vilket återspeglar dess ursprung i lågnivå, systemprogrammering där maximal prestanda och kontroll nära maskinvaran är kritiska. Istället antar C en mer manuell approach till felhantering som passar dess filosofi att ge programmerare så mycket kontroll som möjligt, även på bekostnad av bekvämlighet.

Även om denna metod ligger väl i linje med C:s designmål, kan den också leda till utförlig kod för felkontroll och potentialen för missade felkontroller, vilket moderna språk adresserar med strukturerade undantagshanteringsmekanismer. Till exempel tillåter undantag i språk som Java eller C# centraliserad felbehandling, vilket gör koden renare och felhanteringen mer rakt på sak. Dock introducerar undantag egna överhuvuden och komplexitet, vilket kanske inte är idealiskt för systemnivåprogrammering där C utmärker sig.

Trots dess grovhet har denna manuella felhantering i C informerat designen av felhantering i många andra språk, vilket erbjuder en modell där explicita felförhållanden kan leda till mer förutsägbar och felsökbar kod. För kritiska system, där fel måste hanteras nådigt, säkerställer C:s felhanteringsparadigm - kombinerat med moderna bästa praxis som felhanteringsbibliotek och konventioner - robusthet och tillförlitlighet.
