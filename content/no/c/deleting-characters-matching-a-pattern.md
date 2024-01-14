---
title:    "C: Sletting av tegn som matcher et mønster"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være nødvendig å slette bestemte tegn eller tekstmønstre fra en tekststreng i et program. Dette kan for eksempel være for å fjerne uønsket informasjon eller for å formatere teksten på en spesiell måte. Uansett årsak, er det viktig å vite hvordan man kan utføre denne handlingen i et C-program.

## Slik gjør du det

For å slette tegn eller tekstmønstre fra en tekststreng i C, kan man bruke funksjonen "strtok". Denne funksjonen deler opp tekststrengen i mindre biter basert på et gitt tegn eller mønster, og returnerer hvert stykke som en egen streng.

Her er et eksempel på hvordan man kan bruke "strtok" til å slette alle forekomster av et bestemt tegn i en tekststreng:

```C
#include <stdio.h> 
#include <string.h> 
  
int main() 
{ 
    char tekst[] = "dette er en tekst med noen a'er"; 
    char *del = "a"; 
    char *stykke; 
  
    // bruker strtok til å dele opp tekststrengen 
    stykke = strtok(tekst, del); 
  
    while (stykke != NULL) { 
        printf("%s ", stykke); 
        stykke = strtok(NULL, del); // går videre til neste stykke 
    } 
  
    return 0; 
}
```

Dette eksempelet vil gi følgende output:

```dette er en tekst med noen 'er```

Som du kan se, vil alle "a" i tekststrengen være fjernet.

## Dykk dypere

Det finnes flere måter å slette tegn eller tekstmønstre på i C, for eksempel ved bruk av "strchr" eller "strstr" funksjonene. Det kan også være nyttig å ha kunnskap om hvordan man bruker regulære uttrykk når man skal slette flere tegn eller komplekse mønstre.

Husk også at når man sletter tegn eller tekstmønstre, vil lengden på tekststrengen bli endret. Dette kan påvirke andre deler av koden, spesielt dersom du jobber med dynamiske tekststrenger. Derfor er det viktig å være forsiktig og ha god kunnskap om hvordan de forskjellige funksjonene virker.

## Se også

- [C-programmering](https://www.w3schools.in/c-tutorial/)
- [strtok funksjonen](https://www.tutorialspoint.com/c_standard_library/c_function_strtok.htm)
- [strchr funksjonen](https://www.geeksforgeeks.org/strchr-c-library-function/)
- [strstr funksjonen](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [Regulære uttrykk i C](https://www.copyleft.no/portal/coding/c-programmeringsverktoy-2/regulare-uttrykk-regex-i-c)