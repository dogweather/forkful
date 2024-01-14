---
title:    "C: Omvandling av en sträng till gemener"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver kan vara en viktig del av en programmörs arbete. Det gör det möjligt för användaren att ange input på olika sätt och få samma resultat. Det kan också vara användbart för att filtrera eller jämföra strängar i ett program.

## Hur du gör det

Du kan enkelt konvertera en sträng till små bokstäver i C genom att använda funktionen `tolower` från standard biblioteket `<ctype.h>`. Den här funktionen tar en karaktär som argument och returnerar den motsvarande små bokstaven om den är en stor bokstav, annars returnerar den bara samma karaktär. Här är ett exempel på hur det kan göras:

```C
#include <stdio.h>
#include <ctype.h>

int main(void) {
    char input[] = "Hej VÄRLDEN!";
    
    printf("Input: %s\n", input);
    
    // Loopa igenom strängen och konvertera varje karaktär
    for (int i = 0; input[i] != '\0'; i++) {
        input[i] = tolower(input[i]);
    }
    
    printf("Output: %s\n", input);
    
    return 0;
}
```

Output: `hej världen!`

## Fördjupning

Vad händer egentligen i bakgrunden när du konverterar en sträng till små bokstäver? När du använder `tolower` funktionen, skickas karaktären till en specialfunktion som kallas för `locale-specific mapping`. Denna funktion används för att bestämma det korrekta motsvarande små bokstaven i det språk som användaren har valt för sitt system. Detta säkerställer att din kod fungerar korrekt för alla användare oavsett deras språkinställningar.

## Se även

- [C Programlama Dili (Turkish): Case Sensitivity](https://www.turkcede.net/c-kucuk-buyuk-harf-duyarliligi/)

- [The Basics of C Programming: Character Manipulation](https://www.youtube.com/watch?v=eUhLTLZusAM)

- [C String Library Functions](https://www.programiz.com/c-programming/c-standard-library-functions/string-h)