---
title:    "Swift: Konvertere en dato til en streng"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Mange ganger, som en Swift-programmerer, kan du stå overfor behovet for å konvertere en dato til en streng. Dette kan være nyttig for å vise datoer i en visuell måte, eller for å lagre datoer i en database som bare godtar tekstverdier. Uansett hva årsaken må være, er det en enkel prosess å konvertere en dato til en streng i Swift-programmeringsspråket.

# Hvordan

For å konvertere en dato til en streng i Swift, kan du bruke "DateFormatter" -klassen som setter opp formateringen av datoen. Du kan spesifisere ønsket format ved hjelp av "dateFormat" -egenskapen til denne klassen. La oss se på et eksempel:

```Swift
let dato = Date()
let datoFormatter = DateFormatter()
datoFormatter.dateFormat = "dd.MM.yyyy"
la konvertertDato = datoFormatter.string(from: dato)

print(konvertertDato)
// Output: 18.06.2021
```
I dette eksemplet har vi laget en ny datoobjekt ved hjelp av "Date()" -funksjonen. Deretter har vi opprettet en instans av "DateFormatter" -klassen for å konvertere datoen til en streng. Vi har satt formatet til å være "dd.MM.yyyy", som betyr at datoen vil bli vist som dag/måned/år. Til slutt har vi brukt "string(from:)" -metoden for å konvertere datoen til en streng og lagre den i en variabel som heter "konvertertDato". Denne strengen blir deretter skrevet ut og resultatet vil være datoen som en streng i ønsket format.

# Deep Dive

I tillegg til "dateFormat" -egenskapen, har "DateFormatter" -klassen flere andre nyttige metoder og egenskaper for å hjelpe deg med å formatere datoer. Noen av dem inkluderer "localizedString(from:)" -metoden som lar deg formatere datoen basert på brukerens lokale preferanser, og "dateStyle" og "timeStyle" egenskapene som gir deg muligheten til å spesifisere et standardisert visningsformat basert på dato og tid. For en dypere dykk inn i konvertering av datoer til strenger, kan det være nyttig å lese dokumentasjonen til DateFormatter-klassen.

Se også

- [Swift Date and Time Tutorial](https://www.raywenderlich.com/7795929-swift-date-and-time-tutorial-getting-started)
- [Apple Developer Documentation for DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)