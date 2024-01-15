---
title:                "Satunnaislukujen luominen"
html_title:           "Go: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi?

On monia syitä, miksi ohjelmoijat haluavat käyttää satunnaislukugeneraattoria. Se voi olla hyödyllistä esimerkiksi pelien kehittämisessä tai salauksen luomisessa.

## Miten?

Go-kielessä voit helposti luoda satunnaislukugeneraattorin käyttämällä "math/rand" -kirjastoa ja sen "rand.Intn" -funktiota. Voit antaa halutun lukuluokan parametrina ja ohjelma palauttaa satunnaisen luvun kyseiseltä alueelta.

```Go
import "fmt"
import "math/rand"

func main() {
  // Generoidaan satunnainen luku väliltä 1-100
  randomNum := rand.Intn(100) + 1
  fmt.Println("Satunnainen luku on:", randomNum)
}
```

Tämä koodiesimerkki tulostaa satunnaisen luvun väliltä 1-100. Voit muuttaa parametria ja testata erilaisia luvunalueita.

## Syväsukellus

Go-kielessä käytetty satunnaislukugeneraattori tulee "math/rand" -kirjastosta, mikä perustuu algoritmiin nimeltä "Mersenne Twister". Tämä algoritmi on yksi suosituimmista menetelmistä satunnaislukujen tuottamiseen ja se on myös hyvin tehokas. "math/rand" -paketti myös tarjoaa muita funktioita ja metodeja satunnaislukujen generoimiseen, kuten "Float32" ja "Float64".

On myös tärkeää huomata, että satunnaislukugeneraattoreita ei pidä käyttää salausmenetelmiin tai muuhun turvallisuustarkoitukseen. Niitä tulee käyttää vain simulaatioissa ja muissa vastaavissa tarkoituksissa.

## Katso myös

- "math/rand" -kirjaston dokumentaatio: https://golang.org/pkg/math/rand/
- Satunnaislukugeneraattori Wikipedia-sivulla: https://fi.wikipedia.org/wiki/Satunnaislukugeneraattori