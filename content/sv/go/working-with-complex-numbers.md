---
title:                "Att arbeta med komplexa tal"
date:                  2024-01-26T04:41:11.478091-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med komplexa tal"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Komplexa tal, som består av en reell del och en imaginär del (som 5 + 7i), är avgörande inom områden såsom ingenjörsvetenskap, fysik och signalbehandling. Programmerare arbetar med dem för att lösa problem inom dessa domäner som skulle vara svåra att knäcka med bara reella tal.

## Hur gör man:
Go har inbyggt stöd för komplexa tal. Här är en snabb genomgång:

```go
package main

import (
	"fmt"
	"math/cmplx"
)

func main() {
	// Skapa komplexa tal
	a := complex(2, 3)
	b := 4 + 5i

	// Grundläggande operationer
	fmt.Println("Addition:", a+b)
	fmt.Println("Subtraktion:", a-b)
	fmt.Println("Multiplikation:", a*b)
	fmt.Println("Division:", a/b)

	// Egenskaper hos komplexa tal
	fmt.Println("Reell del:", real(b))
	fmt.Println("Imaginär del:", imag(b))
	fmt.Println("Konjugat:", cmplx.Conj(b))
	fmt.Println("Storlek:", cmplx.Abs(b))
	fmt.Println("Fasvinkel (radianer):", cmplx.Phase(b))
}

```

Exempel på utmatning:

```
Addition: (6+8i)
Subtraktion: (-2-2i)
Multiplikation: (-7+22i)
Division: (0.5609756097560976+0.0487804878048781i)
Reell del: 4
Imaginär del: 5
Konjugat: (4-5i)
Storlek: 6.4031242374328485
Fasvinkel (radianer): 0.8960553845713439
```

## Djupdykning
För länge sedan betraktades komplexa tal med misstänksamhet - vissa trodde att de var meningslösa! Med tiden blev deras kraft i att beskriva fysiska fenomen tydlig. De är grundläggande inom kvantfysik, reglerteknik och elektroteknik, för att nämna några områden.

I Go representeras komplexa tal med en datatyp som kallas `complex128` (64 bitar för reell och imaginär del vardera) eller `complex64` (32 bitar vardera). Bakom kulisserna är dessa verkligen bara två `float64`s eller `float32`s som sitter ihop. Gos standardbibliotek, `math/cmplx`, erbjuder funktioner för komplexa matematikoperationer. Detta sparar dig från det trassliga matematiken och låter dig fokusera på att lösa problem.

Alternativ till Gos inbyggda stöd inkluderar att använda externa bibliotek eller att skapa egen hantering av komplexa tal. Men dessa är sällan nödvändiga eftersom Gos infödda stöd är effektivt och välintegrerat i språket.

## Se också
Kolla in dessa länkar för mer om Gos förmågor med komplexa tal:
- Gos officiella dokumentation: https://golang.org/pkg/math/cmplx/
- En djupare matematikgenomgång av komplexa tal: https://www.mathsisfun.com/numbers/complex-numbers.html
- Praktiska tillämpningar av komplexa tal inom ingenjörsvetenskap: https://ieeexplore.ieee.org/document/528dunno