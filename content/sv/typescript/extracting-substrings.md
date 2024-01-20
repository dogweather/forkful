---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera understrängar handlar om att hämta specifika delar av en sträng. Programmerare gör det för att manipulera data, oftast för filtrering, sökning eller jämförelser.

## Hur man gör:
Här är några exempel på att extrahera understrängar i TypeScript.

```TypeScript
let text: string = "Hej, Världen!";
let subText1: string = text.slice(0,3);
console.log(subText1);
// Output: "Hej"

let subText2: string = text.substring(0,3);
console.log(subText2);
// Output: "Hej"

let subText3: string = text.substr(0,3);
console.log(subText3);
// Output: "Hej"
```

`slice()`, `substring()` och `substr()` är tre vanliga metoder för att extrahera understrängar. Alla dessa metoder returnerar en ny sträng och ändrar inte den ursprungliga strängen.

## Fördjupning:
Extrahering av understrängar har sina rötter i äldre programmeringsspråk som C och C++. Även om det finns flera metoder för att uppnå samma sak, som `slice()`, `substring()` och `substr()`, används de på olika sätt och har olika beteenden i olika situationer.

`slice()` accepterar negativa index, medan `substring()` inte gör det. Istället byter `substring()` plats på de två argumenten om den första är större än den andra. `substr()` låter det andra argumentet ange antalet tecken att returnera istället för slutgiltigt index.

Betrakta följande:

```TypeScript
let text: string = "Hej, Världen!";
console.log(text.slice(-1));
// Output: "!"
console.log(text.substring(-1));
// Output: "Hej, Världen!"
console.log(text.substr(-1));
// Output: "!"
```
## Se även:
UTF-8 kodning och Unicode-tecken kan påverka extraheringen av understrängar. Följande länkar ger mer information:

- [MDN Web Docs - String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN Web Docs - String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs - String.prototype.substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [UTF-8 och Unicode](https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/)