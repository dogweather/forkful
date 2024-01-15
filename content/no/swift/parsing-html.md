---
title:                "Analysering av html"
html_title:           "Swift: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å analysere HTML? Vel, ved å parse HTML kan du trekke ut relevant informasjon fra nettsider og gjøre den tilgjengelig for videre behandling i din app eller nettside.

## Hvordan

Hvis du vil lære å parse HTML ved hjelp av Swift, følg disse trinnene.

1. Importer `Foundation`-biblioteket i din Swift-fil for å få tilgang til `URL`- og `Data`-klassene som vi skal bruke.

```Swift
import Foundation
```

2. Lag en instans av `URL` ved å oppgi URL-adressen til siden du vil analysere.

```Swift
let url = URL(string: "https://www.din-nettside.no")
```

3. Opprett en `Data`-instans ved å laste ned HTML-innholdet fra URL-adressen.

```Swift
let data = try Data(contentsOf: url)
```

4. Bruk `String`-klassens `init(data:encoding)`-metode for å konvertere `Data`-instansen til en lesbar streng.

```Swift
let htmlString = String(data: data, encoding: .utf8)
```

5. Nå kan du bruke Swift's innebygde funksjoner for strenger til å finne og trekke ut informasjon fra HTML-strengen. For eksempel, hvis du ønsker å finne alle lenker på nettsiden, kan du bruke `range(of:)`-funksjonen. Merk at dette vil gi deg indeksene til alle forekomster av den spesifikke strengen du leter etter.

```Swift
let linkRange = htmlString?.range(of: "<a href")
```

6. For å få tilgang til de faktiske lenkene, bruk `substring(from:)` og `substring(to:)`-funksjonene for å klippe ut den delen av strengen du vil ha. For eksempel, hvis du vil ha lenken mellom to anker-tag-er (`<a>...</a>`), kan du bruke denne kodebiten.

```Swift
if let startRange = htmlString?.range(of: "<a href"), let endRange = htmlString?.range(of: "</a>"), let link = htmlString?.substring(with: startRange.upperBound..<endRange.lowerBound) {
    // Din kode for å gjøre noe med lenken
}
```

7. Og voilà, du har nå klart å parse HTML ved hjelp av Swift!

## Dypdykk

Med parsing av HTML kommer også mange utfordringer. HTML er ofte ustrukturert og kan variere mye fra nettside til nettside. Det kan også være vanskelig å finne spesifikke elementer på en nettside hvis de ikke har en unik identifikator. Det er derfor viktig å være tålmodig og eksperimentere med forskjellige måter å finne og trekke ut informasjon fra HTML.

## Se også

- [Apple's offisielle dokumentasjon for Swift](https://swift.org/documentation/)
- [WWDC-presentasjon om HTML-parsing med Swift](https://developer.apple.com/videos/play/wwdc2017/402/?time=1474)