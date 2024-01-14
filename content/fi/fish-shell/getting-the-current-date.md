---
title:                "Fish Shell: Nykyisen päivämäärän saaminen"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Miksi

Miksi kukaan haluaisi hakea nykyisen päivämäärän? Ainakin minulle se on tärkeä tieto, sillä se auttaa minua pysymään ajan tasalla ja suunnittelemaan tulevia tapahtumia. Esimerkiksi jos teen koodiprojektia, haluan ehkä tallentaa projektin päivämäärän tai lisätä sen lähdekoodiin. Päivämäärän hakeminen on myös hyödyllistä kun haluan tarkistaa, milloin jotain tehtävää on viimeksi suoritettu tai milloin hälytys on asetettu.

##Miten

Fish Shellillä on helppo tapa saada nykyinen päivämäärä. Käytä "date" komentoa ja lisää "-u" vaihtoehto, jotta saat yksityiskohtaisemman tuloksen.

```Fish Shell

date -u

```

Tämä komento antaa tulosteeksi nykyisen päivämäärän UTC-aikavyöhykkeessä. Voit myös lisätä muita vaihtoehtoja, kuten "-I" saadaksesi päivämäärän ISO-muodossa tai "-R" saadaksesi sen RFC 2822 -muodossa.

Jos haluat lisätä päivämäärän lähdekoodiisi, voit käyttää "set" komentoa ja tallentaa sen muuttujaan. Esimerkiksi:

```Fish Shell

set current_date (date -u +"%Y-%m-%d")

```

Tämä tallentaa nykyisen päivämäärän muodossa "vuosi-kuukausi-päivä" muuttujaan nimeltä "current_date". Voit sitten käyttää tätä muuttujaa muissa komennoissa tai skripteissäsi.

##Syvä sukellus

Päivämäärän hakeminen Fish Shellillä perustuu "date" komentoon, joka käyttää UNIX-timestampia, eli lukua, joka edustaa aikaa sekunteina 1. tammikuuta 1970 klo 00.00 UTC:sta. Tätä lukua käytetään laskemaan päivämäärä ja aika eri formaatteihin. Lisää tietoa tästä löydät Fish Shellin manuaalista komennolla "man date".

##Katso myös

- [Fish Shellin manuaali](https://fishshell.com/docs/current/index.html)
- [Päivämäärän hakeminen Bashilla](https://devconnected.com/how-to-get-current-date-and-time-in-bash/)
- [Päivämäärän muotoilu POSIX-standardilla](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/posix.txt)