---
title:                "Päivämäärän jäsennys merkkijonosta"
aliases: - /fi/bash/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:00.496655-07:00
model:                 gpt-4-0125-preview
simple_title:         "Päivämäärän jäsennys merkkijonosta"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

Päivämäärän jäsentäminen merkkijonosta Bashissa tarkoittaa päivämäärätiedon poimimista ja muuntamista tekstiaineistosta muotoon, jota Bash voi käsitellä tai käyttää jatkoprosesseissa. Tämä on yleinen vaatimus skriptaustehtävissä, kuten lokitiedostojen analysoinnissa, tiedostojen järjestämisessä päivämääräleimojen perusteella tai automatisoidussa raportoinnissa, mikä tekee siitä olennaisen taidon ohjelmoijille aikatiedon tehokkaaseen hallintaan ja hyödyntämiseen.

## Miten:

Bash itse on melko rajallinen suorissa päivämäärän jäsentämiskyvyissä ja se nojaa usein ulkoisiin työkaluihin, kuten `date` ja `awk`, monimutkaisempiin manipulaatioihin. Tässä on, miten voit jäsentää tietyn formaatin ja sitten käyttää sitä `date`-komennon kanssa, jotta voit muuntaa sen tai suorittaa operaatioita.

**Esimerkki 1:** Poimi päivämäärämerkkijono ja muunna se toiseen formaattiin.

Oletetaan, että sinulla on päivämäärä muodossa `vvvv-kk-pp` ja haluat muuntaa sen muotoon `pp-kk-vvvv`.

```bash
alkuperainen_paivamaara="2023-04-01"
muotoiltu_paivamaara=$(date -d $alkuperainen_paivamaara '+%d-%m-%Y')

echo $muotoiltu_paivamaara
```

**Esimerkkitulo:**
```
01-04-2023
```

Tämä käyttää `date`-komentoa `-d`-vaihtoehdon kanssa määrittämään syöttöpäivämäärämerkkijonon, ja `+%d-%m-%Y` muotoilemaan tulosteen.

**Esimerkki 2:** Käyttämällä `awk`:ia jäsentämään päivämäärä rakenteellisesta tekstirivistä ja muuntamaan se.

Oletetaan, että sinulla on lokitiedostorivi:

```
2023-04-01 12:00:00 Käyttäjä kirjautui sisään
```

Voit poimia ja muuntaa päivämääräosan käyttämällä `awk`:ia ja `date`:a.

```bash
loki_rivi="2023-04-01 12:00:00 Käyttäjä kirjautui sisään"
paivamaara_osa=$(echo $loki_rivi | awk '{print $1}')
muotoiltu_paivamaara=$(date -d $paivamaara_osa "+%A, %B %d, %Y")

echo $muotoiltu_paivamaara
```

**Esimerkkitulo:**
```
Lauantai, huhtikuu 01, 2023
```

Tässä esimerkissä käytetään `awk`:ia jakamaan lokirivi ja poimimaan päivämääräosa (`$1` edustaa ensimmäistä välilyönnein eroteltua kenttää), ja sitten `date`:a käytetään sen uudelleenmuotoiluun.

### Kolmannen osapuolen työkalujen käyttö

Monimutkaisempia jäsentämisiä tai monenlaisten päivämäärämuotojen käsittelyä varten kolmannen osapuolen työkalut, kuten `dateutils`, voivat olla erittäin käteviä.

**Esimerkki `dateutils`-ohjelmalla:**

Oletetaan, että sinulla on päivämäärämerkkijono ei-standardissa muodossa, esimerkiksi, `huhtikuu 01, 2023`.

```bash
alkuperainen_paivamaara="huhtikuu 01, 2023"
muotoiltu_paivamaara=$(dateconv -i "%B %d, %Y" -f "%Y-%m-%d" <<< $alkuperainen_paivamaara)

echo $muotoiltu_paivamaara
```

**Esimerkkitulo:**
```
2023-04-01
```

Tämä komento käyttää `dateutils`-pakettiin kuuluvaa `dateconv`:ia, määrittäen syöttömuodon `-i`-vaihtoehdolla ja halutun tulostusmuodon `-f`-vaihtoehdolla. `dateutils` tukee laajaa valikoimaa päivämäärä- ja aikamuotoja, tehden siitä erittäin monipuolisen välineen päivämäärän jäsentämistehtäviin Bash-skripteissä.
