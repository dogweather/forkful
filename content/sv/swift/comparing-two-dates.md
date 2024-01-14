---
title:    "Swift: Jämföra två datum"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

När man skriver kod är det ofta nödvändigt att jämföra två datum. Det kan vara för att kontrollera om ett datum är större än ett annat, eller om de är lika. Att kunna göra detta korrekt är avgörande för att skapa robusta och tillförlitliga program. I denna blogginlägg kommer jag att gå igenom hur man jämför två datum på ett effektivt sätt med hjälp av Swift.

## Så här gör du

För att jämföra två datum i Swift, kommer vi att använda oss av Date-klassen. Vi kan skapa två datumobjekt med hjälp av Date-konstruktorn och sedan jämföra dem med hjälp av olika jämförelseoperatorer. Här är ett exempel på hur du jämför två datum för att se om de är lika:

```Swift
let datum1 = Date()
let datum2 = Date(timeIntervalSinceNow: 3600) // Skapar ett datum som är en timme framåt i tiden från nu

if datum1 == datum2 {
  print("Datumen är lika")
} else {
  print("Datumen är inte lika")
}
```
Output: Datumerna är inte lika

Vi kan också använda oss av andra operatorer som till exempel > (större än) och < (mindre än) för att jämföra datum. Här är ett exempel:

```Swift
let nyligen = Date()
let ettÅrSedan = Date(timeIntervalSinceNow: -31536000) // Skapar ett datum som är ett år tillbaka i tiden från nu

if nyligen > ettÅrSedan {
  print("Datumet är senare än ett år sedan")
}
```
Output: Datumet är senare än ett år sedan

## Djupdykning

När vi jämför datum i Swift, används inte bara datumvärdet utan även tidszonen och kalenderinställningar. Det är därför viktigt att vara medveten om dessa faktorer när man utför jämförelser. Om man till exempel vill jämföra två datum på olika tidszoner kan man först konvertera dem till UTC innan man jämför dem för att få ett korrekt resultat.

Det finns även andra fördjupade koncept som kan användas när man jämför datum, som till exempel DateFormatter och Calendar-klassen. Calendar-klassen erbjuder möjligheten att utföra mer avancerade jämförelser som att se om två datum är på samma dag, vecka eller månad.

## Se även

Här är några användbara resurser för att lära dig mer om att jämföra datum i Swift:

- [Officiell Swift dokumentation om Date](https://developer.apple.com/documentation/swift/date)
- [Tutorial om att jämföra datum i Swift på raywenderlich.com](https://www.raywenderlich.com/767-how-to-compare-dates-in-swift)
- [En artikel om hantering av tidszoner och datum i Swift](https://theswiftdev.com/2017/10/03/how-to-handle-time-zones-dates-in-swift/)

Förhoppningsvis har detta blogginlägg hjälpt dig att förstå hur man jämför datum i Swift på ett enkelt och effektivt sätt. Lycka till med dina kodprojekt!