---
title:                "TypeScript: Användning av reguljära uttryck"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför du borde använda reguljära uttryck i TypeScript

Reguljära uttryck är ett kraftfullt verktyg för strängmanipulering i TypeScript. Genom att använda reguljära uttryck kan du enkelt söka, extrahera och ersätta text i en sträng på ett effektivt sätt. Detta sparar både tid och möda jämfört med manuell manipulation av strängar.

## Så här använder du reguljära uttryck i TypeScript

För att använda reguljära uttryck i TypeScript, måste du först skapa ett reguljärt uttryck objekt. Detta kan göras med hjälp av den inbyggda RegExp-klassen:

```TypeScript
let regex = new RegExp("mönster");
```

Här ersätter "mönster" med det reguljära uttrycket du vill använda. Du kan även använda en så kallad "literal" notation för att skapa ett reguljärt uttryck:

```TypeScript
let regex = /mönster/;
```

För att söka efter en matchning i en sträng, använder du metoden `test` på ditt reguljära uttryck objekt:

```TypeScript
let str = "Detta är en teststräng";
let regex = /test/;
console.log(regex.test(str)); // Output: true
```

För att extrahera en del av en sträng som matchar det reguljära uttrycket, använder du metoden `exec`:

```TypeScript
let str = "Min adress är 123 Gatan";
let regex = /\d+/; // Denna regexp matchar vilket heltal som helst
let result = regex.exec(str);
console.log(result[0]); // Output: 123
```

Du kan också använda reguljära uttryck för att ersätta text i en sträng. Använd metoden `replace` på strängen och skicka in det reguljära uttrycket och byt ut-strängen som argument:

```TypeScript
let str = "Jag gillar att äta äpplen";
let regex = /äpplen/;
console.log(str.replace(regex, "päron")); // Output: Jag gillar att äta päron
```

## Djupdykning i reguljära uttryck i TypeScript

Reguljära uttryck i TypeScript följer stort sett samma syntax som i andra programmeringsspråk som Javascript eller Python. Men det finns några skillnader som är värda att notera.

En sak att tänka på är att reguljära uttryck är "globala" som standard i TypeScript, vilket betyder att de kommer att söka efter alla matchningar i hela strängen. Om du bara vill hitta den första matchningen, måste du använda flaggan `^` som betyder att matchningen börjar i början av strängen.

```TypeScript
let str = "Hej jag heter Johan";
let regex = /jag/;
console.log(str.match(regex)); // Output: [ 'jag', index: 8, input: 'Hej jag heter Johan' ]
regex = /^jag/;
console.log(str.match(regex)); // Output: null
```

Det finns också många olika specialtecken och kvantifikatorer som kan användas i reguljära uttryck för att göra sökningen ännu mer exakt. Det kan ta en stund att lära sig alla dessa, men det är definitivt värt det när du behöver göra komplexa sökningar i stora mängder text.

## Se även

Här är några länkar för dig som vill lära dig mer om reguljära uttryck i TypeScript:

- [MDN Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Regular Expression Documentation](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Regex101](https://regex101.com) (för att testa och experimentera med reguljära uttryck)