---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Reaaliaikaisen päivämäärän hankinta on toiminto, jonka avulla ohjelmat saavat nykyisen päivämäärän ja kellonajan tietyn ajovyöhykkeen mukaisesti. Käyttötarkoituksia ovat muun muassa aikaleimojen luominen, tehtävien ajoitus tai tapahtumien lokitiedot.

## Näin se toimii:

Bash-syntaksi komentoa varten on melko yksinkertainen. Voit saada nykyisen päivämäärän käyttämällä `date` komentoa.

```Bash
date
```

Tämän pitäisi tulostaa jotakin, mikä näyttää tämän kaltaiselta (muuttuu ajan perusteella):

```Bash
Ke Heinä  7 14:30:55 EEST 2021
```

Jos haluat mukauttaa tulosteen muotoa, voit käyttää plusmerkkiä + ja tarjoamalla erityisiä muotoilukoodeja. 

```Bash
date +"%Y-%m-%d"
```

Tämä tulostaa päivämäärän yleisessä muodossa:

```Bash
2021-07-07
```

## Syvällisempää tarkastelua:

Historiallisesti `date` komento alkoi mukaan UNIX-ympäristössä 1970-luvulla. Korvaavia komentoja voivat olla esimerkiksi `ncal` ja `cal`, jotka näyttävät kuukausia ja vuosia, mutta ne eivät ole yhtä joustavia kuin `date`.

Bashin `date` komento tulkitsee syötteet C-kirjaston `strftime`-funktion mukaisesti. Voit käyttää moninaisia muotoilukoodeja esittämään päivämäärätietoja monilla yksityiskohtaisilla tavoilla.

## Lue lisää:

- Man-sivu `date`: https://man7.org/linux/man-pages/man1/date.1.html
- GNU Core Utilities (`date` sisältyy): https://www.gnu.org/software/coreutils/coreutils.html
- strftime-funktion dokumentaatio: https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#index-strftime