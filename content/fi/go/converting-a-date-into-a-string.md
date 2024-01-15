---
title:                "Päivämäärän muuttaminen merkkijonoksi."
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi."
simple_title:         "Päivämäärän muuttaminen merkkijonoksi."
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Miksi

Päivämäärän muuttaminen merkkijonoksi (tekstimuotoon) voi olla tarpeellista erilaisissa ohjelmointiprojekteissa, kuten käyttäjän näkymien luomisessa tai tietokannan tallennuksessa. Go:n sisäänrakennettu aikakoodin käyttöliittymä tekee tästä konversion todella helpoksi.

# Kuinka tehdä

Voimme käyttää `time`-pakettia ja sen `Format`-funktiota muuttaaksemme päivämäärän merkkijonoksi.

```
import (
    "fmt"
    "time"
)

func main() {
    date := time.Now()
    stringDate := date.Format("02.01.2006")
    fmt.Println(stringDate)
}
```

Tässä esimerkissä olemme muuttaneet nykyisen päivämäärän "dd.mm.yyyy" muotoon. Voit vaihdella `Format`-funktion parametreja muuttaaksesi päivämäärän erilaisiin merkkijonomuotoihin. Esimerkiksi "Mon, January 2 2006" tulostaisi päivämäärän muodossa "Ma, tammikuu 2 2006".

# Syvällisempi tarkastelu

`Format`-funktio käyttää Go:n sisäänrakennettua aikakoodimallia "reference time". Tämä malli tarjoaa tarkkoja indikaattoreita päivämäärän ja ajan esitysmuodoille. Jokaiselle aikayksikölle on oma kirjaintunnus, esimerkiksi "02" tarkoittaa kuukauden ensimmäistä nollatymppiin päättyvää päivää ja "2006" tarkoittaa vuotta.

On myös mahdollista käyttää muita `Format`-funktion parametreja, kuten `Monthonum` tai `Month` saadaksesi kuukauden numeron tai nimen. Voit lukea lisää eri kirjaintunnusten käytöstä Go:n virallisesta dokumentaatiosta. 

# Katso myös

- [Go:n `time`-paketin virallinen dokumentaatio](https://golang.org/pkg/time/)
- [Go:n `Format`-funktion parametrien käyttö](https://golang.org/pkg/time/#Time.Format)