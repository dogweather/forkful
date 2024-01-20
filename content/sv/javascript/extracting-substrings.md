---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att extrahera delsträngar (substrings) refererar till processen av att ta en specifik sekvens av tecken från en sträng. Programmörer gör det för att manipulera data för olika syften - exempelvis, att dela upp användarens inmatning eller att tolka och bearbeta text.

## Såhär gör du:

Du kan extrahera en delsträng i Javascript med metoden `slice()`, `substring()` eller `substr()`. Här är exempel på hur varje metod fungerar:

```Javascript
let str = 'Hej, världen!';

console.log(str.slice(0, 3));   // Output: "Hej"  
console.log(str.substring(0, 3)); // Output: "Hej" 
console.log(str.substr(0, 3));   // Output: "Hej" 
```

Alla tre metoder ger samma resultat i det här fallet, men de skiljer sig åt i hur de hanterar första och andra argumentet.

## Djupdykning:

Historiskt sett introducerades `slice()` och `substring()` först i javascript, medan `substr()` kom senare. `substr()` anses dock vara förlegad och rekommenderas inte för användning i nya projekt.

De viktigaste skillnaderna mellan metoderna är deras beteende vid negativa värden och deras andra argument. `slice()` och `substr()` accepterar negativa tal, varvid de räknar från slutet av strängen, medan `substring()` gör negativa tal till noll. `slice()` och `substring()` använder det andra argumentet som en indexposition, medan `substr()` använder det som längden på delsträngen.

Vi rekommenderar att du föredrar `slice()` över de andra två metoderna i de flesta fallen på grund av dess förmåga att hantera negativa tal.

## Se även:

För mer djupgående diskussion och jämförelser mellan olika delsträngsmetoder, se följande artiklar och goldbiter:

3. [JavaScript String prototype property on W3Schools](https://www.w3schools.com/js/js_string_methods.asp)