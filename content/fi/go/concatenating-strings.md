---
title:                "Go: Joukkojen yhdistäminen"
simple_title:         "Joukkojen yhdistäminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa sisäkkäisten tekstien (strings) yhdistäminen on hyödyllistä. Esimerkiksi, kun haluat luoda dynaamisen viestin käyttäjälle, joka sisältää käyttäjän antaman tiedon.

## Miten

```Go
func main() {
    greeting := "Hei "
    name := "Sara"
    message := greeting + name
    fmt.Println(message)
}
```

Tulostus:

```
Hei Sara
```

Käyttämällä `+` operaattoria voidaan yhdistää kaksi tai useampia tekstisuureita luoden uuden tekstin. Gootässä ei ole erillistä operaattoria tekstin yhdistämiseen, kuten joissakin muissa kielissä, vaan `+` operaattori toimii myös tässä tarkoituksessa.

## Syvällisempää tietoa

Go:ssa tekstien yhdistäminen on tehokasta ja nopeaa. Takana oleva syy on se, että teksti (string) on kiinteä sekvenssi tavuja, joten se voidaan liittää suoraan ilman ylimääräistä allokointia.

On hyvä huomata, että Go:ssa tekstit ovat muuttumattomia, mikä tarkoittaa, ettei alkuperäistä tekstiä voida muokata tekstien yhdistämisen jälkeen. Sen sijaan yhdistämisen tuloksena luodaan aina kokonaan uusi teksti. Tämä voi aiheuttaa ongelmia esimerkiksi suurilla tekstimäärillä.

## Katso myös

- [Go tekstikäsittely (string manipulation) dokumentaatio](https://golang.org/pkg/strings/)
- [Tekstin manipulointi Go:ssa](https://blog.bethge.org/2019/03/08/text-manipulation-go/)