---
title:    "C: Radera tecken som matchar ett mönster"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför
Ibland kan det vara nödvändigt att ta bort vissa tecken i en sträng som matchar ett visst mönster. Detta kan bero på olika skäl, som till exempel att rensa bort skräp eller ogräs från en sträng eller att modifiera en befintlig sträng för en specifik applikation.

## Hur man gör
Att ta bort tecken som matchar ett visst mönster i en C-program är relativt enkelt. Först behöver vi definiera vår sträng som en array av tecken och sedan loopa igenom den. I varje iteration kontrollerar vi om tecken i strängen matchar vårt önskade mönster och tar bort dem om de gör det.

```C
#include <stdio.h>

int main() {
    char str[] = "Hej alla mördare!";
    char letter = 'a';

    for (int i = 0; str[i] != '\0'; i++) {
        if (str[i] == letter) {
            for (int j = i; str[j] != '\0'; j++) {
                str[j] = str[j + 1];
            }
            i--;
        }
    }

    printf("%s", str);

    return 0;
}
```
Koden ovan kommer att ta bort alla förekomster av tecknet 'a' från strängen "Hej alla mördare!" och skriva ut "Hej ll mördre!". Genom att ändra värdet på variabeln "letter" kan vi enkelt ta bort andra tecken.

## Djupdykning
Att ta bort tecken som matchar ett visst mönster kan också utföras på andra sätt, till exempel genom användning av inbyggda funktioner som strchr() eller strstr(). Det finns också bibliotek som erbjuder mer avancerade mönstermatchningsverktyg, som regex.h.

En annan viktig faktor att tänka på är prestanda när man tar bort tecken från en sträng. Om strängen är lång och det finns många förekomster av det matchande mönstret kan det vara mer effektivt att använda en annan algoritm, till exempel genom att skapa en ny sträng utan de matchande tecknen istället för att manipulera den befintliga strängen.

## Se även
- [strchr() i C++](https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm)
- [strstr() i C++](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [Regex i C](https://www.geeksforgeeks.org/regex-functions-in-c-syntax-usage/)
- [Effektivitetsjämförelse av olika algoritmer för att ta bort tecken från en sträng](https://www.geeksforgeeks.org/remove-characters-from-the-first-string-which-are-present-in-the-second-string/)