---
title:                "C: Muuntamassa päivämäärää merkkijonoksi"
simple_title:         "Muuntamassa päivämäärää merkkijonoksi"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi: Miksi muuttaa päivämäärä merkkijonoksi?

Päivämäärän muuttaminen merkkijonoksi on tärkeää ohjelmointitehtävissä, joissa tarvitaan päivämäärän esittämistä käyttäjälle tai tallentamista tietokantaan. Tämä toiminto mahdollistaa päivämäärän muokkaamisen ja esittämisen halutulla tavalla, joka voi vaihdella eri kielialueilla tai käyttäjien mieltymysten mukaan.

## Kuinka tehdä: Koodiesimerkkejä ja tulosteita "```C ... ```" koodilohkoissa.

Päivämäärän muuttaminen merkkijonoksi tapahtuu usein C-kielellä käyttäen hyödyksi olemassa olevia funktioita, kuten "sprintf" tai "strptime". Koodiesimerkeissä näytetään erilaisia tapoja muuttaa päivämäärä halutussa muodossa ja tulosteet osoittavat, miten merkkijono näkyy ohjelmassa.

```C
#include <stdio.h>
#include <time.h>

int main() {
	// Luo tietorakenne, joka sisältää päivämäärän tiedot
	struct tm date = {
		.tm_mday = 5, // Päivämäärä
		.tm_mon = 11, // Kuukausi, joulukuu on 11
		.tm_year = 120, // Vuosi 2020 on 120 vuotta ajanlaskun alusta
	};
	
	// Muunna päivämäärä merkkijonoksi halutussa muodossa
	char str_date[50];
	sprintf(str_date, "%d.%d.%d", date.tm_mday, date.tm_mon+1, 1900+date.tm_year);
	
	printf("Päivämäärä muutettuna merkkijonoksi: %s", str_date);
	
	return 0;
}

// Tuloste: Päivämäärä muutettuna merkkijonoksi: 5.12.2020
```

## Syventävä tieto: Lisää tietoa päivämäärän muuttamisesta merkkijonoksi.

Tässä artikkelissa esitetyt koodiesimerkit ovat vain lyhyt esimerkki siitä, kuinka päivämäärä voidaan muuttaa merkkijonoksi. Syvempää ymmärrystä varten kannattaa tutustua C-kielellä saatavilla oleviin funktioihin ja niiden eri muotoihin. Muistettavia asioita ovat myös päivämäärän muuntaminen eri aikavyöhykkeille ja huomioitavaa on myös vuosiluvun esitystapa ja sen vaikutus tulokseen.

## Katso myös: Linkkejä lisätietoon päivämäärän muuttamisesta merkkijonoksi.

- [C strftime -funktio](https://www.cplusplus.com/reference/ctime/strftime/)
- [Päivämäärän muuntaminen eri aikavyöhykkeille](https://www.ibm.com/support/knowledgecenter/fr/SSLTBW_2.4.0/com.ibm.zos.v2r4.bpxb500/strptime_t.htm)
- [Vuoden esitystavan vaikutus](https://en.cppreference.com/w/c/chrono/time_print)