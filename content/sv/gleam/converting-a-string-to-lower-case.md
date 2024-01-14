---
title:    "Gleam: Konvertera en sträng till små bokstäver"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till gemener (lower case) är en vanlig uppgift inom programmering, och detta kan ha flera olika användningsområden. Det kan till exempel vara för att göra jämförelser mellan strängar mer exakta, eller för att förenkla sökningar i en databas.

## Hur man gör det
För att konvertera en sträng till gemener i Gleam, kan du använda funktionen `String.to_lower()` tillsammans med din sträng som argument. Exempelvis:

```Gleam
let str = "Hej! Detta är en Sträng"
let lower_str = String.to_lower(str)
```

Detta skulle ge följande output: `"hej! detta är en sträng"`

## Djupdykning
När man konverterar en sträng till gemener, så ändras alla bokstäver i strängen till små bokstäver. Detta innebär att alla bokstäver i exempelvis svenska åäö också kommer konverteras till gemener. Det är även viktigt att notera att konverteringen endast påverkar bokstäver och inte andra tecken, såsom siffror eller specialtecken.

En annan viktig aspekt att tänka på är att konverteringen är fallbaserad, vilket innebär att om du har en blandning av gemener och versaler i din sträng, så kommer detta behållas vid konverteringen. Exempelvis skulle strängen `"Helloworld"` bli `"helloworld"`, men `"HelloWorld"` skulle fortfarande vara `"HelloWorld"` efter att ha använt `String.to_lower()`.

## Se även
- [Gleams dokumentation för strängar](https://gleam.run/documentation/stdlib.html#string)
- [En guide för grundläggande Gleam programmering](https://medium.com/greatest-engl/guide-to-gleam-programming-language-5df502d49c7)
- [Mer om konvertering av strängar i Gleam](https://www.connekt.com/blog/getting-started-with-gleam#converting-strings)