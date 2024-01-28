---
title:                "Kompleksilukujen käsittely"
date:                  2024-01-26T04:40:47.948912-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kompleksilukujen käsittely"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Kompleksiluvut, jotka koostuvat reaali- ja imaginaariosasta (kuten 5 + 7i), ovat keskeisiä aloilla kuten insinöörityö, fysiikka ja signaalinkäsittely. Ohjelmoijat käyttävät niitä ongelmien ratkaisemiseen näillä aloilla, mitä olisi vaikea tehdä pelkillä reaaliluvuilla.

## Kuinka:
Golla on sisäänrakennettu tuki kompleksiluvuille. Tässä pikakatsaus:

```go
package main

import (
	"fmt"
	"math/cmplx"
)

func main() {
	// Kompleksilukujen luominen
	a := complex(2, 3)
	b := 4 + 5i

	// Perustoiminnot
	fmt.Println("Lisäys:", a+b)
	fmt.Println("Vähennys:", a-b)
	fmt.Println("Kertolasku:", a*b)
	fmt.Println("Jakolasku:", a/b)

	// Kompleksiluvun ominaisuudet
	fmt.Println("Reaaliosa:", real(b))
	fmt.Println("Imaginaariosa:", imag(b))
	fmt.Println("Konjugaatti:", cmplx.Conj(b))
	fmt.Println("Suuruus:", cmplx.Abs(b))
	fmt.Println("Vaihekulma (radiaanit):", cmplx.Phase(b))
}

```

Esimerkkituloste:

```
Lisäys: (6+8i)
Vähennys: (-2-2i)
Kertolasku: (-7+22i)
Jakolasku: (0.5609756097560976+0.0487804878048781i)
Reaaliosa: 4
Imaginaariosa: 5
Konjugaatti: (4-5i)
Suuruus: 6.4031242374328485
Vaihekulma (radiaanit): 0.8960553845713439
```

## Syväsukellus
Aikoinaan kompleksilukuja katsottiin epäluulolla — jotkut ajattelivat niiden olevan hyödyttömiä! Ajan mittaan niiden voima fyysisten ilmiöiden kuvaamisessa kävi selväksi. Ne ovat perustavanlaatuisia kvanttifysiikassa, säätöteoriassa ja sähkötekniikassa, vain muutamia aloja mainitaksemme.

Gossa kompleksilukuja kuvataan käyttämällä tietotyyppiä `complex128` (64 bittiä sekä reaali- että imaginaariosalle) tai `complex64` (32 bittiä kummallekin). Pohjimmiltaan nämä ovat vain kaksi `float64`:ää tai `float32`:ta yhdessä. Gon standardikirjasto, `math/cmplx`, tarjoaa funktioita kompleksilaskutoimituksille. Tämä säästää sinut monimutkaiselta matematiikalta ja antaa keskittyä ongelmien ratkaisuun.

Vaihtoehtoja Gon sisäänrakennetulle tuelle sisältävät ulkoisten kirjastojen käytön tai oman kompleksilukujen käsittelyn luomisen. Mutta näitä harvoin tarvitaan, koska Gon natiivi tuki on tehokas ja hyvin integroitu kieleen.

## Katso myös
Tutustu näihin linkkeihin saadaksesi lisätietoja Gon kompleksilukutoiminnoista:
- Gon virallinen dokumentaatio: https://golang.org/pkg/math/cmplx/
- Syvempi matematiikan kertaus kompleksiluvuista: https://www.mathsisfun.com/numbers/complex-numbers.html
- Kompleksilukujen käytännön sovellukset insinöörityössä: https://ieeexplore.ieee.org/document/528dunno
