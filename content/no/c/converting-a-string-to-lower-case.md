---
title:                "Konvertere en streng til små bokstaver"
html_title:           "C: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville noen ønske å konvertere en streng til små bokstaver i C? Vel, det kan være mange grunner til dette. Kanskje du ønsker å sammenligne to strenger, men ønsker ikke at store og små bokstaver skal påvirke resultatet. Eller kanskje du ønsker å formatere utdataen din på en mer enhetlig måte. Uansett hva grunnen er, er det enkelt å konvertere en streng til små bokstaver i C-programmering.

## Hvordan gjøre det

Det finnes flere måter å konvertere en streng til små bokstaver i C på, men her er en enkel metode:

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main()
{
    char str[] = "Hei NORGE!";
    int length = strlen(str);
    
    for (int i = 0; i < length; i++) {
        if (isupper(str[i])) {
            str[i] = tolower(str[i]);
        }
    }
    
    printf("%s", str); // utskrift: hei norge!
    
    return 0;
}
```

La oss ta en nærmere titt på koden. Vi starter med å inkludere headerfilene "stdio.h", "string.h" og "ctype.h". Deretter definere vi en variabel "str" som inneholder den opprinnelige strengen vi ønsker å konvertere. "Length" er en variabel som inneholder lengden på strengen, som vi trenger for å iterere gjennom den. I løkken sjekker vi hver bokstav i strengen ved hjelp av "isupper()" -funksjonen, som sjekker om bokstaven er en stor bokstav. Hvis den er det, bruker vi "tolower()" -funksjonen til å konvertere den til en liten bokstav. Til slutt skriver vi ut den nye strengen ved hjelp av "printf()" -funksjonen.

Du kan også bruke bibiotekfunksjonen "strlwr()" for å konvertere en streng til små bokstaver. Denne funksjonen tar imot en streng som parameter og returnerer en konvertert streng. Her er et eksempel på hvordan du kan bruke den:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "Hei NORGE!";
    
    printf("%s", strlwr(str)); // utskrift: hei norge!
    
    return 0;
}
```

Det er viktig å merke seg at denne funksjonen bare fungerer for ASCII-tegn. Hvis du trenger å konvertere en streng med multibyte- eller Unicode-tegn, må du bruke en annen metode.

## Utforske dypere

Nå som vi har sett på to enkle måter å konvertere en streng til små bokstaver i C på, kan vi se litt nærmere på hvordan disse funksjonene faktisk fungerer. "tolower()" og "strlwr()" -funksjonene bruker begge ASCII-tabellen for å konvertere bokstavene. Hver bokstav har et numerisk verdi tilordnet seg, og denne verdien endres for å konvertere bokstaven til en liten bokstav. For eksempel, i ASCII-tabellen er "A" lik 65 og "a" er lik 97. Ved å legge til 32 i verdien til en stor bokstav, får vi den tilsvarende lille bokstaven.

Om du trenger å konvertere en streng med multibyte- eller Unicode-tegn, må du bruke en annen metode som tar hensyn til disse tegnsystemene. Du kan for eksempel bruke "setlocale()" -funksjonen for å sette det lokale tegnsettet til "C", som støtter multibyte-tegn, før du gjør konverteringen.

## Se også

- [toupper() - C biblioteksfunksjon](https://www.programiz.com/c-programming/library-function/ctype.h/toupper)
- [strchr() - C biblioteksfunksjon](https://www.programiz.com/c-programming/library-function/string.h/strchr)