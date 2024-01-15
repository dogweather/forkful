---
title:                "Merkkijonon muuttaminen isolla alkukirjaimella"
html_title:           "Bash: Merkkijonon muuttaminen isolla alkukirjaimella"
simple_title:         "Merkkijonon muuttaminen isolla alkukirjaimella"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Välillä tarvitaan muuttaa tekstin muotoa siten, että ensimmäinen kirjain on isossa kirjaimessa ja loppu pienissä kirjaimissa. Tätä varten tarvitaan koodia, joka pystyy muokkaamaan merkkijonon muotoa. Bash tarjoaa yksinkertaisen ratkaisun tähän tehtävään.

## Kuinka

Käyttämällä ```Bash ``` koodiblokin sisällä olevaa ```tr``` komentoa, voit helposti muuttaa merkkijonon ensimmäisen kirjaimen isoksi ja loput pieniksi. Alla on esimerkkikoodi ja sen tulos.

```Bash
# Luodaan muuttuja, jossa on pienet kirjaimet
teksti="esimerkki teksti"
# Käytetään tr-komentoa muokkaamaan merkkijono
echo $teksti | tr '[:lower:]' '[:upper:]'
```

Tulos: *Esimerkki teksti*

## Syväsukellus

Tr-komennon toiminta perustuu merkkijonojen muokkaamiseen yksinkertaisilla säännöillä. Tässä tapauksessa käytetään sääntöä, joka vaihtaa kaikki pienet kirjaimet suuriksi.

Toinen tapa suorittaa sama tehtävä on käyttää ```bash ``` komentoa ```sed``` yhdessä merkkijonopalvelimen kanssa. Seuraavassa esimerkissä käytetään samaa muuttujaa ja tuloksena saadaan myös *Esimerkki teksti*.

```Bash
# Käytetään sed-komentoa muokkaamaan merkkijonoa
echo $teksti | sed 's/.*/\u&/'
```

## Katso myös

- Bash-ohjeet: https://www.gnu.org/software/bash/manual/
- Tr-komento: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Sed-komento: https://www.gnu.org/software/sed/manual/sed.html