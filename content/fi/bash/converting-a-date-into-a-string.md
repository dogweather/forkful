---
title:    "Bash: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa päivämäärän merkkijonoksi? Päivämäärän muuntaminen merkkijonoksi on hyödyllistä esimerkiksi silloin, kun haluat tallentaa päivämäärän tietokantaan tai käyttää sitä tiedostojen nimissä.

## Miten

Seuraavassa on esimerkki kuinka voit muuttaa päivämäärän merkkijonoksi Bash-skriptillä:

```Bash
# Asetetaan muuttuja muuttamaan päivämäärä tietyssä muodossa
date="2020-04-15"

# Muutetaan päivämäärä merkkijonoksi muodossa "Päivä-Kuukausi-Vuosi"
new_date=$(date -d "$date" +'%d-%m-%Y')

# Tulostetaan uusi muotoiltu päivämäärä
echo $new_date
```

Tämän esimerkkiskriptin tulos on "15-04-2020".

## Syväluotaus

Bash tarjoaa monia tapoja muuttaa päivämäärä eri muotoihin. Käytetyt vaihtoehdot skriptissä ovat "-d" ja "+%". "-d" antaa Bashille tiedon, että kyseessä on päivämäärä, ja "+%" määrittelee muokkauksen tyylin. Voit löytää täydellisen listan näistä muokkausvaihtoehdoista Bashin virallisilta dokumentaatioilta. On myös tärkeää huomata, että tuloksen muodostamisessa käytetyt välimerkit voivat vaihdella käytetyn kielen mukaan. Esimerkiksi Suomessa käytetään "Päivä-Kuukausi-Vuosi"-muotoa, kun taas Yhdysvalloissa käytetään yleisesti "Kuukausi-Päivä-Vuosi"-muotoa.

## Katso myös

- [Bashin viralliset dokumentaatiot](https://www.gnu.org/software/bash/manual/bash.html)
- [Päivämäärän muotoilu Bashissa](https://www.baeldung.com/linux/bash-date-format)