---
title:    "C: Generering av tilfeldige tall"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Generering av tilfeldige tall er en viktig del av programmering, spesielt når det kommer til spill, kryptografi og tilfeldige simuleringer. Ved å kunne generere tilfeldige tall kan vi skape variasjon og uforutsigbarhet i programmene våre, noe som kan være avgjørende for deres funksjonalitet.

## Hvordan gjøre det
I C kan vi bruke srand()-funksjonen for å sette en startverdi for genereringen av tilfeldige tall. Deretter kan vi bruke rand()-funksjonen for å generere et tilfeldig tall. Vi kan også bruke en for-løkke for å generere flere tilfeldige tall.

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // Setter en startverdi for tilfeldige tall
    srand(1234);
    
    // Genererer 10 tilfeldige tall og skriver dem ut
    for(int i=0; i<10; i++){
        int num = rand();
        printf("%d\n", num);
    }
    
    return 0;
}
```

Eksempel på output:
```C
834214
17304
691114
779934
828538
276056
350430
588265
859189
163970
```

## Dypdykk
Ved hjelp av srand() og rand() funksjonene, kan vi kun generere tall basert på algoritmer. For å få et enda mer tilfeldig resultat, kan vi også bruke systemtiden som en startverdi. Dette vil bidra til å gjøre genereringen av tilfeldige tall enda mer unik og uforutsigbar.

Vi kan også avgrense området for de tilfeldige tallene ved å bruke modulusoperatoren (%). For eksempel, hvis vi kun ønsker tilfeldige tall mellom 1 og 100, kan vi bruke følgende kode:

```C
int num = rand() % 100 + 1; // Vil generere tilfeldige tall mellom 1 og 100
```

Det finnes også ulike algoritmer for å generere tilfeldige tall, som for eksempel "Mersenne Twister" eller "Park-Miller-algoritmen". Disse kan være mer effektive og gi et bedre tilfeldig resultat enn standard srand() og rand() funksjoner.

## Se også
- [Hvordan generere tilfeldige tall i Python](https://www.w3schools.com/python/ref_random_randint.asp)
- [Implementere en tilfeldig algoritme i C++](http://www.cplusplus.com/reference/cstdlib/rand/)
- [Mer om tilfeldige tallgenererering i C](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)