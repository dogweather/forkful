---
title:                "Javascript: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Är du nyfiken på hur du kan extrahera delsträngar från en större sträng? Kanske behöver du göra just detta för att lösa ett programmeringsproblem eller bara för att lära dig en ny färdighet. Oavsett anledning är det en användbar funktion att ha under bältet för alla Javascript-utvecklare.

## Hur man gör det

Det finns flera sätt att extrahera delsträngar i Javascript, men ett vanligt tillvägagångssätt är att använda metoden `substring()`. Den här metoden tar två parametrar: startsindex och slutindex.

Enkelt uttryckt motsvarar startsindex positionen för det första tecknet i delsträngen, och slutindex motsvarar positionen för det sista tecknet. Om slutindex inte anges kommer den extraherade delsträngen att sträcka sig från startsindex till slutet av den ursprungliga strängen.

Låt oss titta på några exempel på hur detta fungerar:

```Javascript
let str = "Det här är en exempelsträng";
console.log(str.substring(4, 8)); // Output: här
console.log(str.substring(13)); // Output: exempelsträng
```

Här extraherar vi en delsträng från indexposition 4 till 8 och sedan från indexposition 13 till slutet av strängen.

En annan metod för att extrahera delsträngar är `slice()`. Denna metod fungerar på ett liknande sätt, men har möjligheten att hantera negativa index för att ange positionen från slutet av den ursprungliga strängen. Här är ett exempel på hur `slice()` kan användas:

```Javascript
let str = "Det här är en exempelsträng";
console.log(str.slice(4, 8)); // Output: här
console.log(str.slice(-12, -5)); // Output: exempel
```

Notera hur vi kan ange negativa index för att extrahera delsträngen "exempel" från slutet av strängen.

## Djupdykning

Det finns många tillfällen i programmering då det kan vara användbart att kunna extrahera delsträngar från en större sträng. Till exempel när du behöver hämta delar av en URL-adress, manipulera användarinput eller formatera text på ett specifikt sätt.

Det finns också flera andra metoder för att extrahera delsträngar i Javascript, såsom `substr()` och `split()`, som kan vara användbara för specifika scenarier. Det viktiga är att kunna identifiera när och hur du behöver extrahera delsträngar, och vilken metod som är lämpligast för din situation.

## Se även

Det finns många resurser på nätet för att lära dig mer om hur man extraherar delsträngar i Javascript. Här är några användbara länkar:

- [MDN Documentation on substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Documentation on slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [W3Schools Tutorial on substring() and slice()](https://www.w3schools.com/jsref/jsref_substring.asp)