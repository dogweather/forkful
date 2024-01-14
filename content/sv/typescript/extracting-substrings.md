---
title:    "TypeScript: Extrahering av delsträngar"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Varför extrahera substrängar i TypeScript?

I TypeScript, precis som i andra programmeringsspråk, kan det ibland vara nödvändigt att extrahera en del av en sträng, även kallad en substräng. Detta kan vara användbart när man behöver bearbeta och manipulera strängar för att få önskat resultat. I denna bloggpost kommer vi att utforska hur man extraherar substrängar i TypeScript och varför detta kan vara användbart.

## Så här extraherar du en substräng i TypeScript

För att extrahera en substräng i TypeScript kan vi använda en metod som heter `slice()`. Denna metod tar två parametrar - den första parametern är positionen där substrängen ska börja och den andra parametern är positionen där substrängen ska sluta. Här är ett exempel på hur vi kan använda `slice()` för att extrahera en substräng från en sträng:

```TypeScript
let str = "Hej! Den här bloggposten handlar om TypeScript.";

let substring = str.slice(4, 18);

console.log(substring); // Resultat: "Den här bloggposten"
```

I detta exempel extraherar vi substrängen "Den här bloggposten" från den ursprungliga strängen genom att ange startpositionen `4` (som motsvarar positionen för bokstaven "D") och slutpositionen `18` (som motsvarar positionen för bokstaven "n"). Observera att den andra parametern är exkluderande, vilket innebär att den inte kommer att inkluderas i den resulterande substrängen.

Förutom att ange positionerna manuellt kan vi också använda en negativ position för att extrahera en substräng från slutet av strängen. Till exempel kan vi använda `str.length` för att extrahera de sista 5 karaktärerna från strängen:

```TypeScript
let str = "Detta är en sträng med många karaktärer.";

let substring = str.slice(-5);

console.log(substring); // Resultat: "ärter"
```

I detta exempel extraherar vi de sista 5 karaktärerna från strängen genom att använda en negativ position `-5` som motsvarar de fem sista karaktärerna "ärter".

## Djupdykning i substrängsextraktion

Förutom `slice()` finns det också andra sätt att extrahera substrängar i TypeScript. Till exempel kan vi använda metoden `substring()` som fungerar på samma sätt som `slice()` men den tar bara en startposition som parameter. Vi kan också använda `substr()` som tar en startposition och en längd på substrängen som parametrar.

Det är också viktigt att notera att substrängsextraktion i TypeScript är icke-destruktiv, vilket innebär att den ursprungliga strängen inte förändras när vi extraherar en substräng. Istället returneras en helt ny sträng som innehåller den önskade substrängen.

## Se också

- [TypeScript String API](https://www.typescriptlang.org/docs/handbook/string.html)
- [Substring extraction in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)