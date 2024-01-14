---
title:    "Swift: Konvertere en dato til en streng"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor
Hvis du noen gang har programmert i Swift, har du sannsynligvis støtt på situasjoner der du må konvertere en dato til en streng. Dette kan være nyttig for å vise datoen i et annet format, for eksempel i en tekstboks eller på et skjermbilde. I denne bloggposten vil jeg gå gjennom hvordan man enkelt kan konvertere en dato til en streng i Swift.

## Slik gjør du det
Konvertering av en dato til en streng i Swift er veldig enkelt, takket være innebygde funksjoner. Her er et eksempel på hvordan du kan gjøre det:

```Swift
// Opprett en Date-variabel med dagens dato
var date = Date()

// Definer en DateFormatter
let formatter = DateFormatter()

// Angi ønsket datoformat
formatter.dateFormat = "dd.MM.yyyy"

// Bruk formateren til å konvertere datoen til en streng
let dateString = formatter.string(from: date)

// Skriv ut resultatet
print(dateString)  // '09.04.2021'
```

Som du kan se, er koden ganske enkel. Først oppretter vi en Date-variabel med dagens dato. Deretter definerer vi en DateFormatter og angir det ønskede datoformatet. Til slutt bruker vi formateren til å konvertere datoen til en streng, som vi deretter skriver ut. Du kan enkelt tilpasse formatteren til å passe dine behov, for eksempel ved å legge til klokkeslett eller endre språket på datoen.

## Dykk dypere
Hvis du ønsker å forstå litt mer om hvordan konvertering av datoer fungerer i Swift, kan det være lurt å lære om DateFormatter-klassen og dens egenskaper og metoder. Her er noen ressurser som kan hjelpe deg med å forstå dette bedre:

- [Apple's offisielle dokumentasjon om DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [En grundig tutorial om datoformatering i Swift](https://learnappmaking.com/dateformatter-swift-how-to-format-dates-code-quickstart/)
- [En forklaring på hvordan dumping av Unicode-språk og regioner fungerer i formatering av datoer](https://nshipster.com/nslocale/)

## Se også
- [How to Work with Dates and Times in Swift](https://www.raywenderlich.com/12652423-how-to-work-with-dates-and-times-in-swift)
- [Mastering Dates and Times in Swift](https://www.swiftbysundell.com/articles/mastering-dates-and-times-in-swift/)
- [Five tips for working with dates in Swift](https://sarunw.com/posts/five-tips-for-working-with-date-in-swift/)