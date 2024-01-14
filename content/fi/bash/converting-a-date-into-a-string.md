---
title:                "Bash: Muuntaminen päivämääräksi merkkijonona"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Harkitset ehkä päivämäärän muuttamista merkkijonoksi Bash-ohjelmointikielessä, koska haluat tulostaa päivämäärän lukijalle ymmärrettävässä muodossa. Päivämäärän muuttaminen merkkijonoksi voi myös olla tarpeellista päivämäärän käsittelyssä osana isompia Skriptejä tai sovelluksia.

## Miten

Voit helposti muuttaa päivämäärän merkkijonoksi Bashissa käyttämällä `date` -komennon `+%Y%m%d` -muotoilua. Seuraavassa on yksi esimerkki:

```Bash
tanaan=$( date +%Y%m%d )
echo $tanaan # tulostaa esimerkiksi "20201127"
```

Jos haluat tarkempaa muotoilua, voit käyttää myös muita `date` -komennon vaihtoehtoja, kuten `+%d.%m.%Y`, joka tulostaa päivämäärän muodossa "27.11.2020". Voit myös muuttaa muuttujan, johon päivämäärä tallennetaan, nimeä ja käyttää sitä toistamiseen myöhemmin skriptissä.

## Syvällinen tutkimus

Päivämäärää käsitellessä on tärkeää muistaa, että se tallentaa nykyisen päivämäärän ja ajan, kun se suoritetaan. Jos haluat tallentaa tulevan tai menneen päivämäärän, sinun on käytettävä muita vaihtoehtoja, kuten `--date` -vaihtoehtoa.

Voit myös muotoilla päivämäärän haluamallasi tavalla vaihtamalla `%Y%m%d` -osan muotoilua vastaavaan muotoiluun. Jokainen merkki tai yhdistelmä edustaa eri osaa päivämäärästä ja ajan kohdalla voit käyttää myös `+%H%M%S` -vaihtoehtoa.

## Katso myös

- [`date` -komennon virallinen dokumentaatio](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Bash Scripting Tutorial](https://linuxhint.com/bash_scripting_tutorial_beginners/)