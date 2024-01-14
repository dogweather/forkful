---
title:    "TypeScript: Generera slumpmässiga nummer"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför generera slumpmässiga nummer

Att generera slumpmässiga nummer används ofta inom programutveckling för att skapa variation och realism i applikationer. Det kan till exempel användas för att simulera tärningskast eller spelresultat.

## Hur man genererar slumpmässiga nummer i TypeScript

För att generera slumpmässiga nummer i TypeScript finns det flera metoder tillgängliga beroende på vilken typ av nummer som ska genereras. Här är några exempel med tillhörande kodblock:

Generera ett heltal mellan 1 och 10:

```TypeScript
Math.floor(Math.random() * 10) + 1;
```

Generera ett decimaltal mellan 0 och 1:

```TypeScript
Math.random();
```

Generera ett slumpmässigt tal med hjälp av ett intervall:

```TypeScript
let min = 50;
let max = 100;
Math.floor(Math.random() * (max - min + 1)) + min;
```

För att få en bättre förståelse för hur koden fungerar kan du testa den i din konsol eller använda ett debugger-verktyg.

## Djupare dykning i generering av slumpmässiga nummer

Metoden Math.random() i TypeScript använder sig av en pseudo-slumpgenerator som genererar nummer baserat på en startpunkt, även kallad seed. Detta innebär att om samma seed används kommer samma sekvens av slumpmässiga nummer att genereras. För att förhindra detta kan du använda en extern slumpgenerator som seedar sig själv för varje anrop, exempelvis genom att använda tiden som seed.

En annan viktig aspekt att ta hänsyn till är att de flesta slumpgeneratorer genererar nummer med en viss fördelning, som vanligtvis är en jämnt fördelad fördelning. Detta innebär att det finns en större chans att ett nummer upprepas än att ett annat nummer genereras. Om du behöver en viss fördelning av nummer måste du använda en anpassad slumpgenerator eller skapa din egen algoritm.

## Se även

- [Math.random](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Slumpmässiga tal i programmeringsspråket Python](https://www.python.org/dev/peps/pep-0524/)
- [Den bästa metoden för att generera slumpmässiga nummer i Java](https://stackoverflow.com/questions/868522/random-number-generation-excluding-specific-values/869157#869157)