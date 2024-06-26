---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:53.673028-07:00
description: "Miten: Go:ssa merkkijonoja kohdellaan muuttumattomina tavujonoina. Voit\
  \ selvitt\xE4\xE4 merkkijonon pituuden k\xE4ytt\xE4m\xE4ll\xE4 sis\xE4\xE4nrakennettua\
  \ `len()`-funktiota,\u2026"
lastmod: '2024-03-13T22:44:56.040705-06:00'
model: gpt-4-0125-preview
summary: Go:ssa merkkijonoja kohdellaan muuttumattomina tavujonoina.
title: "Merkkijonon pituuden m\xE4\xE4ritt\xE4minen"
weight: 7
---

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
