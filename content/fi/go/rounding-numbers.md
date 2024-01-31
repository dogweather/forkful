---
title:                "Numerojen pyöristäminen"
date:                  2024-01-26T03:45:19.863800-07:00
model:                 gpt-4-0125-preview
simple_title:         "Numerojen pyöristäminen"

category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Numeroiden pyöristäminen tarkoittaa luvun viilausta lähimpään kokonaislukuun tai määriteltyyn desimaalikohtaan. Sitä tehdään arvojen yksinkertaistamiseksi, niiden luettavuuden parantamiseksi tai tiettyihin rajoituksiin sopimiseksi, kuten valuuttoja käsiteltäessä.

## Kuinka:
Go:n `math`-paketista löytyy apua pyöristämiseen. Käytä `math.Round`, `math.Floor` ja `math.Ceil` yksinkertaistukseen:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // Pyöristää lähimpään kokonaislukuun
	fmt.Println("Floor:", math.Floor(number)) // Pyöristää alaspäin
	fmt.Println("Ceil: ", math.Ceil(number))  // Pyöristää ylöspäin
}
```

Esimerkkitulostus:
```
Round: 3
Floor: 3
Ceil: 4
```

Tiettyihin desimaalipaikkoihin pyöristäminen: kerro, pyöristä ja jaa:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("Pyöristettynä 2 desimaalipaikkaan:", roundToDecimalPlace(number, 2))
}
```

Esimerkkitulostus:
```
Pyöristettynä 2 desimaalipaikkaan: 3.14
```

## Syväsukellus
Numeroiden pyöristäminen ei ole uutta – se on peräisin muinaismatematiikasta ja tähtää aina yksinkertaisuuteen. Go:n `math.Round` käyttää [pankkirien pyöristystä](https://en.wikipedia.org/wiki/Rounding#Round_half_to_even), mikä tarkoittaa, että 0.5 pyöristetään lähimpään parilliseen numeroon, vähentäen vinoumaa, joka voisi vaikuttaa summiin.

Liukuluvut voivat olla haastavia johtuen niiden binääriesityksestä, joka ei välttämättä täsmällisesti edusta kaikkia desimaaleja. Go:n lähestymistapa kuitenkin ylläpitää odotettua käytöstä suurimman osan ajasta.

Muitakin pyöristysmenetelmiä on olemassa, kuten "pyöristä puoli ylös" tai "pyöristä puoli pois nollasta", mutta Go:n vakio kirjasto tarjoaa valmiin pohjan. Monimutkaisempiin tarpeisiin voi olla tarpeen käyttää kolmannen osapuolen kirjastoa tai kehittää oma ratkaisu.

## Katso myös
- Go:n `math` -paketti: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- IEEE 754 -standardi liukulukuaritmetiikalle (Go:n perusta liukuluvuille): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Liukulukujen ymmärtäminen: ["Mitä jokaisen tietojenkäsittelijän tulisi tietää liukulukuaritmetiikasta"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
