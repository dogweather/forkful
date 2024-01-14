---
title:    "Swift: Slette tegn som samsvarer med et mønster"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Hvorfor

I Swift-programmering er det ofte nødvendig å håndtere ulike tegn og bokstaver i strenger. Noen ganger ønsker man å fjerne visse tegn som samsvarer med et bestemt mønster. Dette kan være nyttig for å rense og formatere data eller for å filtrere ut uønskede tegn før en streng behandles videre.

# Slik gjør du det

For å fjerne tegn som matcher et bestemt mønster, kan du bruke en rekke ulike metoder i Swift. En av de mest brukte metodene er å bruke regular expressions, også kjent som regex. Her er et eksempel på hvordan du kan bruke regex i Swift for å fjerne alle tall fra en streng:

```Swift
let string = "Dette er en streng med tall 1234"
let regex = try! NSRegularExpression(pattern: "[0-9]", options: .caseInsensitive)
let range = NSRange(location: 0, length: string.utf16.count)
let filteredString = regex.stringByReplacingMatches(in: string, options: [], range: range, withTemplate: "")
print(filteredString) // Resultat: "Dette er en streng med tall "
```

I dette eksempelet bruker vi NSRegularExpression-klassen, som gir oss mulighet til å definere et mønster og finne samsvar i en streng. Vi definerer mønsteret [0-9], som betyr alle tall fra 0 til 9. Deretter bruker vi metoden stringByReplacingMatches for å erstatte alle samsvar i strengen med en tom streng.

# Dypdykk

Det finnes ulike måter å definere mønstre på i Swift, avhengig av hvilken versjon av språket du bruker. I tillegg kan det være nyttig å utforske ulike modifieringer for å tilpasse regex-mønstrene dine. For å få en dypere forståelse for hvordan regex fungerer i Swift, kan du utforske dokumentasjonen og prøve ut ulike eksempler.

# Se også

- [Swift Regular Expression Cookbook](https://www.raywenderlich.com/5768-swift-regular-expression-cookbook) 
- [Swift Regular Expressions Cheat Sheet](https://gist.github.com/vinizorza/3915e20bb7cea7a4d406)