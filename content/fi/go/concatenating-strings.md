---
title:    "Go: Merkkijonojen yhdistäminen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi Go-ohjelmoinnin yhteydessä kannattaa yhdistää merkkijonoja? Yhdistämisen avulla voit luoda helposti ja tehokkaasti uusia merkkijonoja, jotka sisältävät erilaista tietoa. Tämä voi olla hyödyllistä esimerkiksi tekstinkäsittelyssä tai tiedostojen nimeämisessä.

## Kuinka

Yhdistämisen suorittamiseen Go-kielessä on useita eri tapoja. Tässä esimerkissä käytämme "+"-operaattoria, joka yhdistää kaksi merkkijonoa toisiinsa.

```Go
// Luodaan muuttuja, joka sisältää ensimmäisen merkkijonon
string1 := "Tämä on ensimmäinen osa "

// Luodaan toinen muuttuja, joka sisältää toisen merkkijonon
string2 := "ja tämä on toinen osa."

// Yhdistetään merkkijonot käyttämällä "+"-operaattoria
mergedString := string1 + string2

// Tulostetaan yhdistetty merkkijono konsoliin
fmt.Println(mergedString)

// Output: Tämä on ensimmäinen osa ja tämä on toinen osa.
```

Voit myös yhdistää merkkijonoja käyttämällä `fmt.Sprintf`-funktiota, joka toimii samalla tavalla kuin C-kielessä käytetty `sprintf`-funktio. Se ottaa vastaan ensimmäisenä parametrina merkkijonon formaatin ja sen jälkeen haluamasi määrän muuttujia, jotka haluat yhdistää uuteen merkkijonoon. Tässä esimerkissä käytämme myös `\n`-merkkiä, joka aiheuttaa rivinvaihdon.

```Go
// Luodaan merkkijono, joka sisältää muuttujan paikan
format := "Tämä on ensimmäinen osa %s ja tämä on toinen osa."

// Yhdistetään merkkijonot käyttämällä fmt.Sprintf-funktiota
mergedString := fmt.Sprintf(format, "muuttujan arvo")

// Tulostetaan yhdistetty merkkijono konsoliin
fmt.Println(mergedString)

// Output: Tämä on ensimmäinen osa muuttujan arvo ja tämä on toinen osa.
```

## Syvemmälle

Concatenating (yhdistäminen) muuttujien sijaan voit myös yhdistää suoraan merkkijonon ja muuttujan. Tämä onnistuu käyttämällä strconv.Itoa (int to ascii) -funktiota, joka muuttaa numeron merkkijonoksi.

```Go
// Luodaan muuttuja, joka sisältää numeron
number := 123

// Yhdistetään merkkijono ja numero käyttämällä strconv.Itoa-funktiota
mergedString := "Numeron arvo on " + strconv.Itoa(number)

// Tulostetaan yhdistetty merkkijono konsoliin
fmt.Println(mergedString)

// Output: Numeron arvo on 123
```

Näissä esimerkeissä käytimme vain `string1 + string2` -muotoa, mutta Go-kielessä on myös mahdollista käyttää `string1 += string2` -muotoa, joka tekee saman asian mutta hieman lyhyemmin.

## Katso myös

- [Go Edition - Strings](https://blog.golang.org/strings)
- [Golang Documentation - String manipulation](https://golang.org/pkg/strings/)
- [Concatenating Strings in Go](https://opensource.com/article/19/9/concatenating-strings-go)