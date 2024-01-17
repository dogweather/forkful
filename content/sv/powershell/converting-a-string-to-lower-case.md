---
title:                "Konvertera en sträng till gemener"
html_title:           "PowerShell: Konvertera en sträng till gemener"
simple_title:         "Konvertera en sträng till gemener"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener är en vanlig process inom programmering. Det innebär helt enkelt att ändra alla bokstäver i en sträng till deras gemena motsvarighet. Detta görs ofta för att underlätta jämförelse av strängar.

## Hur man gör:
```PowerShell
$str = "HeJ VarlDen!"
$str.ToLower()
```
Ger följande utmatning:
`hej världen!`

## Deep Dive:
### Historiskt sammanhang:
Att konvertera strängar till gemener har varit en del av programmering sedan tidiga datorer. Detta beror på att de flesta system har en skillnad mellan gemena och versala bokstäver och det är viktigt att programspråken använder rätt format för att undvika konstiga fel.

### Alternativ:
Det finns flera alternativ för att konvertera strängar till gemener i PowerShell. En annan metod som kan användas är `.ToUpper()` för att göra alla bokstäver versala istället för gemena.

### Implementation:
Konvertering till gemener görs genom att använda metoden `.ToLower()` på en sträng. Detta returnerar en ny sträng med alla bokstäver omvandlade till gemener. Om man vill ändra den ursprungliga variabeln så måste man sätta det nya värdet tillbaka till samma variabel.

## Se även:
- https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/to-lower?view=powershell-7.1 - Officiell dokumentation för `.ToLower()` metod i PowerShell.