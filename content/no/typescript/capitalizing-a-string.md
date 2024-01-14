---
title:    "TypeScript: Store bokstaver i en string"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne kapitalisere en streng (engelsk: capitalize a string) er en viktig ferdighet for enhver som programmerer i TypeScript. Det lar deg endre en tekststreng slik at bare første bokstav i hvert ord er stor, noe som er nyttig for å formatere tekst i ulike applikasjoner.

## Hvordan

Å kapitalisere en streng kan gjøres på flere måter i TypeScript. En av de enkleste metodene er å bruke ".toUpperCase()" funksjonen. La oss se på et eksempel:

```TypeScript
let tekst: string = "dette er en tekst å kapitalisere";

tekst = tekst.replace(/\w\S*/g, function(word) {
    return word.charAt(0).toUpperCase() + word.substr(1).toLowerCase();
});

console.log(tekst);
```

Dette vil outputte: "Dette Er En Tekst Å Kapitalisere". La oss forklare hva som skjer i koden.

Først definerer vi en streng "tekst" som vi ønsker å kapitalisere. Deretter bruker vi metoden ".replace()" for å erstatte alle ord i strengen som starter med en bokstav (vist ved regexp-uttrykket "/\w\S*/g") med en funksjon. Denne funksjonen tar inn hvert ord som en parameter og returnerer ordet med den første bokstaven stor og resten av bokstavene små. Deretter erstatter vi den gamle verdien av "tekst" med den nye verdien ved å tildele den tilbake til variabelen. Til slutt, vi logger ut den nye kapitaliserte strengen med "console.log()". 

En annen måte å kapitalisere en streng på er ved å splitte den opp i et array av ord og deretter mappe dem tilbake til strengen med ".map()" funksjonen. La oss se på et annet eksempel:

```TypeScript
let tekst: string = "dette er en tekst å kapitalisere";

tekst = tekst.split(" ").map(word => word.charAt(0).toUpperCase() + word.slice(1)).join(" ");

console.log(tekst);
```

Dette vil også outputte: "Dette Er En Tekst Å Kapitalisere". Her splitter vi strengen ved mellomrom og mapper hvert ord til en ny streng hvor den første bokstaven blir stor ved hjelp av ".map()" funksjonen. Deretter bruker vi ".join()" funksjonen for å kombinere ordene tilbake til en streng ved å sette mellomrom mellom dem. Til slutt logger vi den kapitaliserte strengen med "console.log()". 

## Deep Dive

Nå som vi har sett på noen måter å kapitalisere en streng i TypeScript, la oss dykke litt dypere inn i hvordan disse metodene fungerer. I første eksempel brukte vi ".replace()" funksjonen med et regexp-uttrykk for å erstatte ordene i strengen vår. Med regexp-uttrykket "/\w\S*/g" henter vi alle ord i strengen som starter med en bokstav. Deretter brukes ".toUpperCase()" funksjonen på første bokstav av hvert ord, og ".substr()" funksjonen på resten av bokstavene for å få strengen tilbake i riktig format.

I det andre eksempelet bruker vi ".split()" funksjonen for å dele strengen ved mellomrom og få et array av ord. Deretter bruker vi ".map()" funksjonen for å endre hvert ord ved å bruke samme logikk som i det første eksempelet. Til slutt, bruker vi ".join()" funksjonen for å kombinere ordene tilbake til en streng.

Det er viktig å merke seg at begge metodene endrer ikke originalstrengen, men heller lager en ny kapitalisert streng. Derfor er det viktig å lagre resultatet av metodene i en variabel for å kunne bruke dem videre i koden.

## Se Også

- [Regular Expressions i TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Metoder for strenger i TypeScript](https://www.w3