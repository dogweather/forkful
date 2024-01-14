---
title:    "Javascript: Stor bokstav i streng"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne manipulere strenger i programmering er en viktig ferdighet for enhver utvikler. En av de vanligste stringoperasjonene er å kapitalisere en streng. Dette betyr at alle små bokstaver blir gjort om til store bokstaver. Men hvorfor trenger vi å gjøre dette? Hva er fordelen med å kapitalisere en streng?

## Hvordan

For å kapitalisere en streng i Javascript, kan du bruke den innebygde metoden `.toUpperCase()`. Denne metoden tar ingen parametere og returnerer en kopi av den originale strengen, men med alle bokstavene gjort om til store bokstaver. La oss se på et eksempel:

```Javascript
let streng = "hei dette er en test";
let nyStreng = streng.toUpperCase();
console.log(nyStreng);
```

Dette vil gi følgende resultat i konsollen:
```
"HEI DETTE ER EN TEST"
```

Som du kan se, ble alle små bokstaver i den originale strengen gjort om til store bokstaver. Dette kan være nyttig når du ønsker å gjøre en streng mer lesbar eller når du ønsker at all input fra brukeren skal være i store bokstaver.

En annen måte å kapitalisere en streng på er å bruke en JavaScript funksjon for å iterere gjennom hver bokstav i strengen og omforme den. La oss se på et eksempel på dette:

```Javascript
function kapitaliserStreng(streng) {
    let kapitalisertStreng = "";
    for (let i = 0; i < streng.length; i++) {
        if (streng[i] === " ") {
            kapitalisertStreng += " ";
        } else {
            kapitalisertStreng += streng[i].toUpperCase();
        }
    }
    return kapitalisertStreng;
}

let testStreng = "dette er en test";
let resultat = kapitaliserStreng(testStreng);
console.log(resultat);
```

Dette vil gi følgende resultat i konsollen:
```
"DETTE ER EN TEST"
```

Her har vi brukt en for-løkke for å iterere gjennom hver bokstav i strengen. Dersom bokstaven er et mellomrom, legger vi det til i den kapitaliserte strengen uten å gjøre noen endringer. Hvis bokstaven derimot er en liten bokstav, bruker vi metoden `toUpperCase()` for å gjøre den om til en stor bokstav og legge den til i den kapitaliserte strengen. Til slutt returnerer vi den kapitaliserte strengen.

## Dypdykk

Når vi skal kapitalisere en streng, kan vi også ta hensyn til språkgenerelle regler. For eksempel er det vanlig å ha små bokstaver i artikler og preposisjoner på norsk. Dersom vi ønsker å følge disse reglene, kan vi bruke en funksjon for å sjekke om bokstaven er en artikkel eller preposisjon før vi gjør den om til en stor bokstav. Her er et eksempel på en slik funksjon:

```Javascript
function kapitaliserStrengNorsk(streng) {
    let artikler = ["den", "det", "en", "et", "de", "som", "til", "i", "på", "av"];
    let kapitalisertStreng = "";
    let ord = streng.split(" ");
    for (let i = 0; i < ord.length; i++) {
        let bokstaver = ord[i].toLowerCase();
        if (artikler.includes(bokstaver)) {
            kapitalisertStreng += bokstaver + " ";
        } else {
            kapitalisertStreng += bokstaver[0].toUpperCase() + bokstaver.slice(1) + " ";
        }
    }
    return kapitalisertStreng.trim();
}

let testStreng = "jeg liker å lese på nynorsk";
let resultat = kapitaliserStrengNorsk(testStreng);
console