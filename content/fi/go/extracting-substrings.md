---
title:    "Go: Alimerkkijonojen erottaminen"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi

Substringien erottaminen on tärkeä osa monia ohjelmointitehtäviä, kuten tekstinkäsittelyä ja tiedonkeruuta. Se mahdollistaa tiedon käsittelemisen osissa ja tekee koodista helpommin hallittavan ja ymmärrettävän. 

## Kuinka tehdä

Substringien erottamiseen on useita tapoja Go-kielessä, mutta yksi yleisimmistä on käyttää `strings` -pakkausta ja sen `Substring` -funktiota. Käytännössä tämä toimii seuraavasti:

```
pääpakkaukseksi "strings"
func main () {
       tekijä := "Go on hieno kieli"
       substring := strings.Substring (tekijä, 3,9)
       fmt.Println (substring)
}
```

Tämä tuottaa seuraavan tulosteen:

`on hieno`

Tässä esimerkissä `3` on alkuperäisen merkkijonon indeksi, josta haluamme aloittaa ja `9` on indeksi, joka osoittaa, kuinka monta merkkiä haluamme kopioida. Voit myös käyttää `len (string)` -funktiota määrittämään loppuindeksi, mikä tekee substringista dynaamisemman.

## Syväsukellus

Go-kielessä substringien erottaminen on suhteellisen helppoa, koska se tarjoaa monia käteviä apufunktioita, kuten `Contains`, `Index` ja `Replace`. Voit myös käyttää `for-loop` -rakennetta käsittelemään useita merkkijonoja yhdellä kertaa ja erottamaan niistä halutut substringit. 

On myös tärkeää muistaa, että merkkijonot ovat muuttumattomia Go-kielessä, mikä tarkoittaa, että substringien palauttamisen sijaan luodaan uusi merkkijono. Tämän vuoksi on tärkeää pitää mielessä suorituskyky ja muistinkäyttö, kun käsitellään isoja merkkijonoja.

## Katso myös

- [Go: string-paketti](https://golang.org/pkg/strings/)
- [Go By Example: Strings](https://gobyexample.com/strings)
- [Merkit uusinnassa: Go-ohjelmointikieli](https://www.youtube.com/watch?v=HvcXs9NEj1E)