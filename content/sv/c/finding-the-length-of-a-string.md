---
title:    "C: Att hitta längden på en sträng"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en grundläggande del av programmering och är viktigt för att kunna manipulera och hantera textdata på ett effektivt sätt. Genom att kunna beräkna längden av en sträng kan man också kontrollera att den inte överstiger en viss storlek eller verifiera att den uppfyller vissa krav.

## Hur man gör det

För att hitta längden av en sträng i C-programmering kan man använda funktionen `strlen`. Denna funktion tar en sträng som argument och returnerar längden av den strängen i form av ett heltal. Här är ett enkelt exempel på hur man kan använda denna funktion:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hej världen";
    int length = strlen(str);
    printf("Längden av strängen är: %d", length);
    return 0;
}
```
Output:
```
Längden av strängen är: 11
```

Det finns också en enklare metod som kan användas om man bara vill impra längden av en sträng utan att spara det i en variabel. Denna metod använder en inbyggd operator - `sizeof` - som kan användas för att bestämma storleken på en variabel. När den används på en sträng, returnerar den storleken på hela strängen, inklusive null-tecknet. Här är ett exempel på hur man kan använda `sizeof` för att hitta längden på en sträng:

```C
#include <stdio.h>

int main() {
    char str[] = "Hej världen";
    int length = sizeof(str)/sizeof(str[0])-1; //subtraherar 1 för att ta bort null-tecknet
    printf("Längden av strängen är: %d", length);
    return 0;
}
```

Output:
```
Längden av strängen är: 11
```

## Djupdykning

När man använder funktionen `strlen`, är det viktigt att veta att den räknar antalet tecken fram till det första null-tecknet. Om din sträng inte har ett null-tecken på slutet, så kommer längden som returneras vara felaktig. Detta kan orsaka problem och buggar i ditt program om du inte är medveten om det.

En annan sak att tänka på är att `sizeof` ger oss storleken på hela strängen som en array av tecken, medan `strlen` returnerar antalet tecken i själva strängen. Detta betyder att om du till exempel har en sträng med 10 tecken, men dess storlek som array är 20 (till exempel på grund av andra variabler som lagras i närheten av den), så kommer `sizeof` att returnera 20, men `strlen` kommer fortfarande bara att returnera 10.

## Se också

- Mer information om `strlen` och `sizeof` finns i C-dokumentationen: https://www.tutorialspoint.com/c_standard_library/string_h.htm
- En guide om C-strängar: https://www.learn-c.org/en/Strings