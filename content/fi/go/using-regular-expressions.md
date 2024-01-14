---
title:                "Go: Säännöllisten lausekkeiden käyttö"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttäisit säännöllisiä lausekkeita Go-ohjelmoinnissa?

Säännölliset lausekkeet ovat voimakas työkalu, jota voit käyttää Go-ohjelmointikielen avulla tietojen etsimiseen, korvaamiseen ja muokkaamiseen. Ne auttavat sinua löytämään ja käsittelemään tietoa, joka ei ole suoraan määritelty tai ei noudata tiettyä muotoa. 

## Kuinka käyttää säännöllisiä lausekkeita Go-ohjelmoinnissa?

Ensinnäkin, sinun täytyy tuoda "regexp" kirjasto käyttämällä `import`-lauseketta. Sen jälkeen voit luoda uuden regexp-olion käyttämällä `regexp.Compile()`-funktiota ja määrittämällä haluamasi säännöllisen lausekkeen sisälle. Katso esimerkkiä alla olevassa koodilohkossa:

```Go
import "regexp"

func main() {
  // Luo uusi regexp-olio määrittelemällä säännöllinen lauseke
  re := regexp.Compile("a[bc]+d")

  // Testaa lauseketta merkkijonossa
  testi := re.MatchString("abbbbbcd")
  fmt.Println(testi) // Tulostaa true
}
```

Aina kun haluat etsiä tai korvata merkkijonossa, voit käyttää `re.FindString()` tai `re.ReplaceAllString()` -funktiota. Katso alla olevaa esimerkkiä:

```Go
import "regexp"

fmt.Println(re.FindString("aeiou")) // Tulostaa "a"
fmt.Println(re.ReplaceAllString("aeiou", "X")) // Tulostaa "Xeiou"
```

## Syvään sukellus säännöllisiin lausekkeisiin

Säännölliset lausekkeet noudattavat tiukkoja sääntöjä ja muotoja, joten niiden käyttöön tutustuminen voi vaatia vähän opiskelua. Tässä muutamia hyödyllisiä linkkejä, jotka auttavat sinua ymmärtämään lisää säännöllisistä lausekkeista ja niiden käytöstä Go-ohjelmoinnissa:

- [Go:n virallinen dokumentaatio säännöllisistä lausekkeista](https://golang.org/pkg/regexp/)
- [Go:n regex-tutoriaali](https://www.golang-book.com/books/intro/10)
- [Go:n regex-sanakirja](https://regex101.com/library/)

## Katso myös

- [Go:n virallinen sivusto](https://golang.org/)
- [Go:n opetusohjelmat ja resurssit](https://austingwalters.com/go-learn-go/)