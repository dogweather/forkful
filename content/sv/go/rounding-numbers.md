---
title:                "Avrundning av tal"
date:                  2024-01-26T03:44:51.874340-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrundning av tal"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att avrunda tal innebär att justera ett tal till sin närmaste hela siffra eller specificerade decimalplats. Det görs för att förenkla värden, göra dem mer läsbara, eller passa dem inom vissa begränsningar, som när man arbetar med valutor.

## Hur man gör:
Go:s `math`-paket är din vän för avrundning. Använd `math.Round`, `math.Floor`, och `math.Ceil` för enkelhetens skull:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Avrunda:", math.Round(number))  // Avrunda till närmaste hela tal
	fmt.Println("Golv:", math.Floor(number)) // Avrunda nedåt
	fmt.Println("Tak: ", math.Ceil(number))  // Avrunda uppåt
}
```

Exempel på utdata:
```
Avrunda: 3
Golv: 3
Tak: 4
```

För specifika decimalplatser, multiplicera, avrunda, sedan dela:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("Avrundad till 2 decimalplatser:", roundToDecimalPlace(number, 2))
}
```

Exempel på utdata:
```
Avrundad till 2 decimalplatser: 3.14
```

## Fördjupning
Att avrunda tal är inte nytt—det går tillbaka till antikens matematik, alltid med målet att förenkla. `math.Round` i Go använder sig av [bankers avrundning](https://en.wikipedia.org/wiki/Rounding#Round_half_to_even), vilket innebär att 0,5 avrundas till det närmaste jämna numret, vilket reducerar en snedvridning som skulle kunna påverka summor.

Flyttal kan vara knepiga på grund av deras binära representation, som kanske inte exakt kan representera alla decimaler. Gos tillvägagångssätt bibehåller dock förväntat beteende för det mesta.

Andra avrundningsmetoder existerar, som "avrunda halvt upp" eller "avrunda halvt bort från noll", men Gos standardbibliotek är vad som är direkt tillgängligt. För mer komplexa behov kan du behöva ett tredjepartsbibliotek eller utveckla din egen lösning.

## Se även
- Go:s `math`-paket: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- IEEE 754-standard för flyttalsaritmetik (Gos grund för hantering av flyttal): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Förstå flyttal: ["Vad varje datavetare bör veta om flyttalsaritmetik"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
