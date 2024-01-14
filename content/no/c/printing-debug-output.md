---
title:    "C: Utskrift av feilsøkingsutdata"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange utviklere vil vurdere å bruke debug utskrift som en måte å feilsøke og forstå programmet sitt på. Ved å skrive ut verdiene av variabler og andre data underveis i programkjøringen, kan man få et bedre bilde av hva som skjer og hvor eventuelle feil oppstår. Dette kan være spesielt nyttig under utvikling og testing av programmer.

## Hvordan

For å skrive ut debug utskrift i C, kan man benytte seg av funksjonen `printf()`. Dette er en veldig nyttig funksjon som tillater deg å skrive ut variabler og tekst til konsollen. La oss se på et enkelt eksempel:

```C
int num = 10;
printf("Verdien av num er: %d \n", num);
```

I dette eksempelet vil `printf()` skrive ut teksten "Verdien av num er: 10" til konsollen. Her bruker vi `%d` som en formateringskode for å angi at vi ønsker å skrive ut en integer verdi. Vi kan også bruke `%f` for å skrive ut float-verdier og `%s` for å skrive ut tekststrenger.

En annen nyttig funksjon i debug utskrift er `fprintf()`, som lar deg skrive ut informasjon til en fil i stedet for konsollen. Dette kan være nyttig hvis man trenger å lagre informasjon for senere analyse.

## Dypdykk

Selv om det kan virke som en enkel og rett frem metode å skrive ut debug informasjon, er det viktig å bruke det med forsiktighet. For mye debug utskrift kan gjøre programmet ditt unødvendig tregt og forvirrende å lese. Derfor kan det være lurt å bruke funksjoner som `#ifdef` og `#define` for å begrense debug utskriften til kun å bli utført under utvikling og testing.

Det er også viktig å huske på at debug utskrift bør fjernes eller kommenteres ut før man leverer programmet sitt til kunden. Dette sikrer at programmet vil kjøre mer effektivt og at kunden ikke blir forvirret av unødvendig utskrift.

## Se Også

- [Guide til Debugging i C](https://www.tutorialspoint.com/cprogramming/c_debugging.htm)
- [10 Debugging Tips for C Programmer](https://hackernoon.com/10-cool-debugging-tips-for-the-c-programmer-c89e8591aa8a)
- [Effektiv Debugging for C++ Utviklere](https://www.pluralsight.com/guides/debugging-cpp)