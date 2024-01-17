---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "Bash: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Käännettäessä päivämäärä merkkijonoksi, Bash-ohjelmointikielen nykyinen versio, luo uusi merkkijonotyyppi, joka sisältää päiväyksen tiedon ja mahdollistaa sen muokkaamisen halutulla tavalla. Tämä on hyödyllistä, sillä ohjelmoijat haluavat usein käyttää päivämääriä osana muuta dataa tai tulostaa ne käyttäjälle ymmärrettävässä muodossa. 

## Miksi ja mitä? 
Päivämäärän muuttaminen merkkijonoksi tarkoittaa, että voit esittää päivämäärän tiedon tekstiä vastaavassa muodossa. Tämä voi sisältää päivän, kuukauden, vuoden ja kellonajan. Ohjelmoijat tekevät tätä, jotta päivämäärä voidaan tallentaa ja käsittää helpommin.

## Kuinka? 
Bash-kielen `date` -komennon avulla voit muuttaa päivämäärän haluamaksesi merkkijonoksi. Katso alla esimerkki, jossa päivämäärä merkitään Englannin kielellä ja muutetaan Suomen kielelle.

```Bash
pvm=en_US date
```
```bash
ma elo 23 20:18:36 UTC 2021
```

```Bash
pvm=en_US date +"%A %d.%m.%Y"
```
```bash
ma 23.08.2021 
```
Voit myös halutessasi tallentaa muokatun päivämäärän muuttujaan tai tulostaa sen suoraan komentoriviltä. Ohjelmointikielen käsittely voi vaihdella, joten tutustu Bashin dokumentaatioon saadaksesi lisätietoja.

## Syvällisempi katsaus 
Ennen Bash-versiota 2.0 päivämäärät esitettiin Unix-järjestelmässä sekunteina, jotka kuluivat tietystä päivämäärästä, jota kutsuttiin "epochiksi". Nykyään yleisin muoto on ISO 8601 -standardin mukainen YYYY-MM-DD-muoto. Bashissa voit myös käyttää muita vaihtoehtoja muokataksesi päivämäärää, kuten `date -d` -komennolla.

## Katso myös 
[Lisätietoja Bashin päivämäärätoiminnosta ja dokumentaatiosta](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameters.html)