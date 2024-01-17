---
title:                "Omvandla en sträng till små bokstäver"
html_title:           "TypeScript: Omvandla en sträng till små bokstäver"
simple_title:         "Omvandla en sträng till små bokstäver"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att konvertera en sträng till gemener (lower case) innebär att alla bokstäver i strängen omvandlas till små bokstäver. Programerare gör detta för att underlätta för användaren att söka och jämföra strängar på ett enhetligt sätt.

# Hur gör man:
```TypeScript

// Exempel 1:
const str = "Hej Världen!";
console.log(str.toLowerCase());
// Output: "hej världen!"

// Exempel 2:
const name = "Sara";
const lowerName = name.toLowerCase();
console.log(`Hej ${lowerName}!`);
// Output: "Hej sara!"
```

# Djupdykning:
Att konvertera strängar till gemener har funnits sedan datorernas tidiga dagar. Det användes vanligtvis för att spara plats och undvika förvirring vid jämförelser. En alternativ metod är att använda metoden `toUpperCase()` för att konvertera alla bokstäver till versaler istället.

När det kommer till implementation använder de flesta språk en inbyggd funktion eller metod för att utföra denna konvertering. I TypeScript, används metoden `toLowerCase()` på en sträng för att returnera en ny sträng med gemener.

# Se även:
- [TypeScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- [TypeScript String Manipulation](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html#string-manipulation-under-the-hood)