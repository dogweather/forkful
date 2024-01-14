---
title:    "Haskell: Omvandla ett datum till en sträng"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Haskell är ett kraftfullt programmeringsspråk som är populärt bland utvecklare på grund av sin funktionella stil och starka typsystem. Något som många utvecklare kanske inte vet är att Haskell också är ett effektivt verktyg när det gäller att konvertera datum till strängar. I denna bloggpost kommer vi att utforska varför du skulle vilja konvertera datum till strängar och hur du kan göra det i Haskell.

## Hur man gör det
För att konvertera datum till strängar i Haskell kan du använda funktionen `show`. Denna funktion omvandlar ett datum till en sträng som följer formatet "YYYY-MM-DD". Här är ett exempel på hur man använder `show`:

```Haskell
import Data.Time.Clock
import Data.Time.Format

currentDate <- getCurrentTime
let stringDate = show (utctDay currentDate)
print stringDate
```

Output:
```
"2019-11-28"
```

Som du kan se i exemplet ovan, behöver du först importera datatyperna `Data.Time.Clock` och `Data.Time.Format` för att kunna använda funktionen `show`. Sedan använder du `getCurrentTime` för att få det aktuella datumet och `utctDay` för att hämta dagen från detta datum. Slutligen använder du `show` för att konvertera dagen till en sträng.

## Djupdykning
Om du vill konvertera datum till en sträng som följer ett annat format, kan du använda funktionen `formatTime`. Du behöver fortfarande importera `Data.Time.Clock` och `Data.Time.Format` men denna gång ska du också importera `System.Locale` för att kunna välja det format du vill använda. Här är ett exempel på hur man använder `formatTime`:

```Haskell
import Data.Time.Clock
import Data.Time.Format
import System.Locale

currentDate <- getCurrentTime
let stringDate = formatTime defaultTimeLocale "%d %b, %Y" currentDate
print stringDate
```

Output:
```
"28 Nov, 2019"
```

I exemplet ovan valde vi formatet "%d %b, %Y" som betyder dagens nummer, månadens förkortning och årets fyra sista siffror. Det finns många andra format som du kan välja från, såsom "%d/%m/%Y" för att få datumet i standard europeiskt format eller "%A, %d %B, %Y" för att få hela datumet utskrivet med dagens namn och månadens namn.

## Se även
- [Haskell Date- och tidsbibliotek](https://github.com/robstewart57/Haskell-Data-Time-Library)
- [Haskell-dokumentation: Data.Time.Format](https://hackage.haskell.org/package/time-1.9.2/docs/Data-Time-Format.html)
- [En introduktion till Haskell för nybörjare](https://codeburst.io/an-introduction-to-haskell-for-beginners-c382af5be24f)