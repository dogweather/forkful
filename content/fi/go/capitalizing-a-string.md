---
title:                "Merkkijonon päätekirjaimen muuttaminen"
html_title:           "Go: Merkkijonon päätekirjaimen muuttaminen"
simple_title:         "Merkkijonon päätekirjaimen muuttaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Saatat huomata, että ohjelmoijat usein käyttävät funktiota, joka muuttaa merkkijonon ensimmäisen kirjaimen isoksi. Tätä kutsutaan "merkkijonon kapitalisoinniksi". Ohjelmoijat tekevät tätä helpottaakseen merkkijonojen käsittelyä ja parantaakseen koodin luettavuutta.

## Kuinka tehdä:

```Go
func kapitalisoi(teksti string) string {
    return strings.ToUpper(teksti[0:1]) + teksti[1:]
}

kapitalisoi("go kieli") // palauttaa "Go kieli"
kapitalisoi("FUNKTIO") // palauttaa "FUNKTIO"
```

## Syvälle sukellus:

Merkkijonon kapitalisointia on käytetty ohjelmoinnissa jo pitkään. Se juontaa juurensa vanhoista kirjoituskoneista, joissa isokirjaimisten kirjainten käyttäminen vaati enemmän voimaa kuin pienikirjaimisten.

On olemassa myös muita tapoja kapitalisoida merkkijonoja, kuten muuttamalla kaikki kirjaimet isoksi tai pieneksi, tai käyttämällä valmiita funktioita kuten `strings.Title`.

Go:n merkkijonojen käsittely perustuu Unicode-standardiin, mikä tarkoittaa että merkkijonon kapitalisoinnissa täytyy ottaa huomioon myös ei-latinalaiset kirjaimet ja merkit. Tämä tekee Go:n kapitalisointifunktiosta erittäin monipuolisen ja käyttökelpoisen kaikenlaisissa ohjelmissa.

## Katso myös:

- [Go:n virallinen dokumentaatio merkkijonojen käsittelyyn](https://golang.org/pkg/strings/)
- [Blogikirjoitus merkkijonon kapitalisoinnista Go:ssa](https://medium.com/@felipedutratine/creating-a-function-to-uppercase-the-first-letter-of-a-string-in-go-b4ed9782304f)