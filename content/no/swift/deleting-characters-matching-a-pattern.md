---
title:    "Swift: Sletting av tegn som matcher et mønster"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger når vi arbeider med tekstbehandling i programmering, møter vi på utfordringer med å fjerne eller endre deler av en tekst som passer et bestemt mønster. Dette kan være en del av en større oppgave, for eksempel å lage en regEx som filtrerer ut uønsket tekst. I denne bloggposten vil jeg dele en enkel metode for å fjerne tegn som matcher et mønster i Swift.

## Slik gjør du

Først må vi definere en streng som inneholder teksten vi ønsker å filtrere. Deretter kan vi bruke en metode som heter `replacingOccurrences(of:with:)` for å slette tegn som matcher et mønster. La oss se på et eksempel:

```Swift
let tekst = "Hei! Håper du har en fin dag!"
let filtrertTekst = tekst.replacingOccurrences(of: "[!?å]", with: "", options: .regularExpression)
print(filtrertTekst)
```

I dette eksempelet ønsker vi å fjerne alle utropstegn, spørsmålstegn og bokstaven "å" fra teksten vår. Vi bruker derfor regEx-uttrykket `"[!?å]"` for å definere dette mønsteret. Når vi kjører koden, vil outputen bli "Hei Håper du har en fin dag", med de uønskede tegnene fjernet.

## Dypdykk

Nå som vi har sett på et enkelt eksempel på å fjerne tegn som matcher et mønster, kan vi gå dypere inn i hvordan dette fungerer. `replacingOccurrences(of:with:)`-metoden tar to parametere: stringen vi ønsker å erstatte og stringen vi ønsker å erstatte den med. I tillegg kan vi bruke `options`-parameteren for å spesifisere hvordan teksten skal behandles. I eksempelet vårt brukte vi `.regularExpression`-alternativet for å definere et regEx-uttrykk.

Vi kan også bruke `replacingOccurrences(of:with:,options:range:)`-metoden for å begrense hvor i teksten vi ønsker å gjøre endringene. Dette kan være nyttig hvis vi for eksempel bare ønsker å fjerne tegnene i en bestemt del av teksten.

## Se også

1. [Apple Developer Documentation for replacingOccurrences(of:with:)](https://developer.apple.com/documentation/foundation/nsstring/1410144-replacingoccurrences)
2. [Swift Regular Expressions Cheat Sheet](https://gist.github.com/telrod/824226)
3. [Hvordan bruke Regular Expressions i Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)

Takk for at du leste denne bloggposten om å fjerne tegn som matcher et mønster i Swift. Jeg håper det var nyttig og at det vil hjelpe deg i fremtidige programmeringsoppgaver! Lykke til videre med Swift-programmeringen!