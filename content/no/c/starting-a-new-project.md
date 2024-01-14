---
title:    "C: Å starte et nytt prosjekt"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor

Å starte et nytt prosjekt kan være en spennende og givende opplevelse for alle som elsker å kode. Det gir muligheten til å skape noe nytt og utforske nye konsepter og teknikker. I tillegg kan det være en flott måte å forbedre sine programmeringsferdigheter på og bygge en portefølje.

## Hvordan å starte et nytt prosjekt i C

Å starte et nytt prosjekt i C kan virke overveldende for noen, spesielt for nybegynnere. Men med riktig fremgangsmåte og verktøy, kan det være en enkel og spennende prosess. La oss se på et enkelt eksempel på hvordan du kan komme i gang med å skrive en "Hallo Verden"-applikasjon i C:

```C
#include <stdio.h>

/* Hovedfunksjonen som skriver ut "Hallo Verden" */
int main() {
   printf("Hallo Verden\n");
   return 0;
}
```

Output:
```
Hallo Verden
```

I dette eksempelet bruker vi standardbiblioteket "stdio.h" som lar oss bruke funksjonen "printf()" til å skrive ut tekst til konsollen. Vi definerer en hovedfunksjon som er påkrevd i alle C-programmer, og inne i denne kaller vi "printf()" for å skrive ut "Hallo Verden". Til slutt returnerer vi verdien 0 for å indikere at programmet ble kjørt uten feil.

## Dykk dypere

Nå som vi har en grunnleggende forståelse av hvordan å starte et nytt prosjekt i C, la oss ta en dypere titt på noen av nøkkelkomponentene som er involvert i prosessen.

- Kompilering: For å kjøre et C-program må det kompileres først. Dette betyr at kildekoden blir konvertert til maskinkode som datamaskinen kan forstå og utføre.
- Verktøy: Det finnes mange verktøy tilgjengelig for å hjelpe deg med å skrive og kjøre C-programmer, som f.eks. en teksteditor, en kompilator og en debugger.
- Biblioteker: Biblioteker inneholder forhåndskrevne funksjoner og kode som kan gjenbrukes i programmer. Dette kan være nyttig for å spare tid og skrive mer effektiv kode.

Å lære mer om disse komponentene og hvordan de fungerer sammen, vil hjelpe deg med å forbedre dine C-programmeringsferdigheter og gi deg en dypere forståelse av prosjektene dine.

## Se også
- [C-programmering for nybegynnere (på norsk)](https://www.ntnu.no/wiki/display/itgk/C-programmering+for+nybegynnere)
- [C-programmering: Enkel innføring (på norsk)](http://folk.uio.no/lmadsen/c-programmering-enkel-innfoering.pdf)
- [Start et nytt C-prosjekt i Visual Studio (på engelsk)](https://docs.microsoft.com/en-us/cpp/build/walkthrough-creating-and-using-a-static-library-cpp?view=vs-2019)