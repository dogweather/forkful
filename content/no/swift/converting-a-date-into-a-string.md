---
title:                "Swift: Konvertering av dato til streng"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor
 
Når du jobber med Swift-programmering og må håndtere datoer, kan du noen ganger trenge å konvertere en dato til en streng for å bruke den i visningen eller lagre i en database. Dette kan virke som en enkel oppgave, men det er viktig å gjøre det på riktig måte for å unngå feil og unødvendig komplisert kode.
 
## Hvordan
 
For å konvertere en dato til en streng, kan du bruke en formateringsstring og DateFormatter-klassen i Swift. Her er et eksempel på hvordan du kan konvertere dagens dato til en streng i "dd/MM/yyyy" -format:
 
```Swift
let date = Date()
 
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let dateString = dateFormatter.string(from: date)
 
print(dateString)
// Output: "17/10/2021"
```
 
Som du kan se, lager vi en instans av DateFormatter og setter formatet vi ønsker å konvertere datoen til. Deretter bruker vi metoden `string(from: )` for å konvertere datoen til en streng basert på det valgte formatet. Dette kan også gjøres med forskjellige formateringsalternativer som å legge til klokkeslett og tidssone.
 
## Dypdykk
 
Hvis du ønsker å lære mer om hvordan DateFormatter fungerer og hvordan du kan tilpasse formateringsstringen din, kan du se nærmere på dokumentasjonen til Apple eller forskjellige ressurser på nettet. Det er viktig å være klar over at DateFormatter også tar hensyn til lokale innstillinger for dato og tid, så det kan være lurt å sjekke dette hvis du har problemer med konverteringen.
 
## Se også
 
- [Dokumentasjon for DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift - Konvertering av dato og streng](https://www.hackingwithswift.com/example-code/language/how-to-convert-dates-and-times-to-a-string-using-dateformatter)
- [Formatere datoer i Swift](https://www.simpleswiftguide.com/how-to-format-dates-in-swift/)