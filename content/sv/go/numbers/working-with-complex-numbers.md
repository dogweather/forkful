---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:03.591246-07:00
description: "Hur: I Go hanteras komplexa tal med hj\xE4lp av de inbyggda funktionerna\
  \ `complex`, `real`, och `imag`, tillsammans med typerna `complex64` och `complex128`\u2026"
lastmod: '2024-03-13T22:44:37.385477-06:00'
model: gpt-4-0125-preview
summary: "I Go hanteras komplexa tal med hj\xE4lp av de inbyggda funktionerna `complex`,\
  \ `real`, och `imag`, tillsammans med typerna `complex64` och `complex128` (som\
  \ representerar 64-bitars och 128-bitars komplexa tal respektive)."
title: Att arbeta med komplexa tal
weight: 14
---

## Hur:
I Go hanteras komplexa tal med hjälp av de inbyggda funktionerna `complex`, `real`, och `imag`, tillsammans med typerna `complex64` och `complex128` (som representerar 64-bitars och 128-bitars komplexa tal respektive). Här är en snabbstartguide:

```go
package main

import (
	"fmt"
)

func main() {
	// Skapa komplexa tal
	a := complex(2, 3) // 2+3i
	b := complex(1, -1) // 1-1i

	// Aritmetiska operationer
	c := a + b
	fmt.Println("Addition:", c) // Utdata: Addition: (3+2i)

	d := a * b
	fmt.Println("Multiplikation:", d) // Utdata: Multiplikation: (5+1i)

	// Åtkomst till reella och imaginära delar
	realPart := real(a)
	imagPart := imag(a)
	fmt.Printf("Reell del: %.1f, Imaginär del: %.1f\n", realPart, imagPart) // Utdata: Reell del: 2.0, Imaginär del: 3.0

	// Komplexkonjugat och magnitud kan beräknas
	conjugate := complex(real(a), -imag(a)) // Manuellt
	fmt.Println("Konjugat av a:", conjugate) // Utdata: Konjugat av a: (2-3i)
}

```

Det här exemplet täcker grunderna, men det finns mycket mer du kan göra med komplexa tal, inklusive att utnyttja paketet `math/cmplx` för mer avancerade operationer som att hitta magnitud, fas och mycket mer.

## Fördjupning
Konceptet med komplexa tal går tillbaka till 1500-talet, men fick bara brett erkännande och rigorös formalisering under 1800-talet. I datorprogrammering har komplexa tal varit en grundpelare för komplex aritmetik i vetenskapliga och ingenjörsmässiga beräkningar sedan de tidiga dagarna. Gos tillvägagångssätt för komplexa tal, genom att göra dem till förstaklassens medborgare med inbyggt stöd och omfattande standardbiblioteksstöd genom paketet `math/cmplx`, sticker ut bland programmeringsspråk. Detta designbeslut återspeglar Gos betoning på enkelhet och prestanda.

Det är dock värt att notera att även om det är kraftfullt att arbeta med komplexa tal i Go, kanske det inte alltid är det bästa tillvägagångssättet för alla tillämpningar, särskilt de som kräver symbolisk matematik eller högprecisionsaritmetik. Språk och miljöer specialiserade på vetenskaplig beräkning, som Python med bibliotek som NumPy och SciPy, eller programvara som MATLAB, kan erbjuda mer flexibilitet och ett bredare utbud av funktioner för specifika tillämpningar.

Det sagt, för systemprogrammering och sammanhang där det är avgörande att integrera beräkningar av komplexa tal i en större, prestandakänslig applikation, erbjuder Gos inbyggda stöd för komplexa tal ett unikt effektivt alternativ.
