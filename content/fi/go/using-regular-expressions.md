---
title:    "Go: Säännöllisten lausekkeiden käyttö"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Go-ohjelmointikielentäjässä?

Säännölliset lausekkeet ovat hyvin voimakas työkalu datan käsittelyyn ohjelmoinnissa, ja niillä on monia käyttötarkoituksia. Niiden avulla voit muun muassa suodattaa, hakea ja muokata tietoja tekstimuodossa olevista tiedostoista ja merkkijonoista. Säännöllisiä lausekkeita käyttämällä voit myös tehostaa ohjelmiesi suorituskykyä ja välttää lukuisia koodirivejä.

## Näin käytät säännöllisiä lausekkeita Go-ohjelmointikielentäjässä

Go tarjoaa sisäänrakennetun paketin säännöllisten lausekkeiden käsittelyyn, joka löytyy `regexp`-nimisestä paketista. Aloittaaksesi säännöllisten lausekkeiden käytön, sinun tarvitsee vain tuoda tämä paketti import-lausekkeen avulla. Seuraavaksi voit käyttää `regexp.MustCompile()`-funktiota luomaan uuden regeksi-ilmaisun, annetun merkkijonon perusteella.

```Go
import "regexp"

func main() {
	re := regexp.MustCompile("a*b+c")
	fmt.Println(re.MatchString("aaabbbc")) // Output: true
	fmt.Println(re.MatchString("ac")) // Output: false
}
```

Koodissa käytetään `re.MatchString(<Merkkijono>)`-funktiota tarkistamaan vastaako merkkijono annettua säännöllistä ilmaisua. Tässä tapauksessa tulosteena saadaan `true` ja `false`, mutta voit myös käyttää muita `regexp`-paketin tarjoamia metodeja, kuten `FindAllString()` ja `FindStringSubmatch()`, valittujen merkkijonojen löytämiseen ja palauttamiseen.

## Syvempi sukellus säännöllisten lausekkeiden maailmaan

Säännöllisten lausekkeiden maailma on laaja ja monimuotoinen, mutta Go tarjoaa useita hyödyllisiä ominaisuuksia niiden käsittelemiseen. Voit esimerkiksi käyttää `\w`-merkintää vastaamaan mihin tahansa kirjaimelliseen tai numeeriseen merkkiin, ja `\d`-merkintää vastaamaan mihin tahansa numeromerkkiin. Voit myös käyttää `+`-merkintää ilmaisemaan, että edeltävää merkkiä tulee olla yhdestä useaan kappaletta.

Toinen tärkeä asia säännöllisiin lausekkeisiin liittyen on tarvittaessa käyttää `()`-merkintöjä ryhmittelyyn, jolloin säännöllisen ilmaisun osia voidaan käsitellä erikseen ja yhdistellä tarvittaessa. Esimerkiksi `ab(cd)ef` vastaa merkkijonoja, joissa on kirjainjono "ab" ja sen jälkeen yksi tai useampi kirjain "cd" ennen loppuosaa "ef".

## Katso myös

- Go-kielen virallinen dokumentaatio säännöllisistä lausekkeista: https://golang.org/pkg/regexp/
- Regular Expressions in Go (säännölliset lausekkeet Go-kielessä), artikkeli O'Reillyn blogissa: https://www.oreilly.com/library/view/regular-expressions-cookbook/9781449327453/ch04s04.html
- O'Reilly School of Technology:n opas säännöllisiin lausekkeisiin Go-kielessä: http://www.ost.edu/pdf/courses/regexp_in_go.pdf