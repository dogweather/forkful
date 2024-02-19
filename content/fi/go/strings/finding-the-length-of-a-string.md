---
aliases:
- /fi/go/finding-the-length-of-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:53.673028-07:00
description: "Merkkijonon pituuden selvitt\xE4minen Go:ssa tarkoittaa sen sis\xE4\
  lt\xE4mien merkkien lukum\xE4\xE4r\xE4n m\xE4\xE4ritt\xE4mist\xE4. Ohjelmoijat suorittavat\
  \ t\xE4t\xE4 toimintoa\u2026"
lastmod: 2024-02-18 23:09:07.074171
model: gpt-4-0125-preview
summary: "Merkkijonon pituuden selvitt\xE4minen Go:ssa tarkoittaa sen sis\xE4lt\xE4\
  mien merkkien lukum\xE4\xE4r\xE4n m\xE4\xE4ritt\xE4mist\xE4. Ohjelmoijat suorittavat\
  \ t\xE4t\xE4 toimintoa\u2026"
title: "Merkkijonon pituuden m\xE4\xE4ritt\xE4minen"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkkijonon pituuden selvittäminen Go:ssa tarkoittaa sen sisältämien merkkien lukumäärän määrittämistä. Ohjelmoijat suorittavat tätä toimintoa säännöllisesti hallitakseen merkkijonoja tehokkaasti, olipa kyse sitten validoinnista, alimerkkijonojen poiminnasta tai yksinkertaisesti käyttäjän syötteiden rajoitusten asettamisesta.

## Miten:
Go:ssa merkkijonoja kohdellaan muuttumattomina tavujonoina. Voit selvittää merkkijonon pituuden käyttämällä sisäänrakennettua `len()`-funktiota, joka palauttaa tavujen lukumäärän, ei välttämättä merkkien lukumäärää. Näin sitä käytetään:

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// Käyttäen len() tavupituuden löytämiseksi
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("Tavupituus:", byteLength) // Tuloste: Tavupituus: 13

	// Hanki tarkka merkkien tai runejen lukumäärä merkkijonossa
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("Rune-pituus:", runeLength) // Tuloste: Rune-pituus: 9
}
```
Ensimmäinen menetelmä `len()`-funktion avulla ei aina anna odotettua tulosta, koska se laskee tavuja. Merkkijonoille, jotka sisältävät ei-ASCII-merkkejä (kuten "世界"), tulee käyttää `unicode/utf8`-paketin `RuneCountInString`-toimintoa, jotta voidaan laskea Unicode-koodipisteet tarkasti.

## Syväsukellus
Ennen Go 1:stä, ei ollut tiukkaa rajanvetoa merkkijonojen käsittelyssä tavujonoina versus merkkijonoina. Post Go 1:ssä, UTF-8:n hyväksyminen merkkijonojen standardikoodausjärjestelmäksi vaati selkeämpiä lähestymistapoja. `len()`-funktio toimii täydellisesti ASCII-merkkijonoille, joissa merkit esitetään yhdessä tavussa. Kuitenkin, kun Go-sovellukset tulivat globaalimmiksi ja tarve tukea lukuisia kieliä ja merkistöjä kasvoi, `len()`-funktion yksinkertaisuuden rajoitukset tulivat näkyviin.

`utf8.RuneCountInString()`-funktion käyttöönotto ja käyttö vastaavat näihin rajoituksiin tarjoamalla tavan laskea todelliset Unicode-merkit (rune Go-termistössä). Tämä menetelmä varmistaa, että pituuden laskenta on riippumaton UTF-8:n koodauksen erityispiirteistä, joissa merkit saattavat kattaa useita tavuja.

Vaihtoehtoinen lähestymistapa merkkijonojen käsittelyyn ja manipulointiin, joka on linjassa Go:n rinnakkaisuuden ja tehokkuuden eetoksen kanssa, saattaisi kohdella merkkijonoja rune-siivuina. Tämä menetelmä kuitenkin edellyttää muunnosvaihetta eikä se välittömästi ratkaise kaikkia Unicode:n monimutkaisuuksia (esim. yhdistävät merkit).

Yhteenvetona, kun `len()` soveltuu tavupituuden mittaamiseen ja on tehokas ASCII-tekstille, `utf8.RuneCountInString()` on luotettavampi valinta maailmanlaajuisesti yhteensopivaan sovellukseen. Kuitenkin kehittäjiä kannustetaan ymmärtämään näiden valintojen vaikutukset suorituskykyyn ja muistin käyttöön.
