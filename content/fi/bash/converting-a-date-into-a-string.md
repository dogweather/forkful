---
title:                "Muunna päivämäärä merkkijonoksi"
html_title:           "Bash: Muunna päivämäärä merkkijonoksi"
simple_title:         "Muunna päivämäärä merkkijonoksi"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitehtävissä saattaa olla tarve muuttaa päivämäärä tekstiksi, esimerkiksi tiedoston luomisessa. Bash-kielen avulla tämä onnistuu helposti ja tässä artikkelissa käymme läpi miten tämä tehdään.

## Kuinka tehdä

Tässä esimerkissä käytämme `date` -komennon avulla muuttaaksemme tämänhetkisen päivämäärän ja ajan tekstimuotoon. Kirjoita seuraava koodi Bash-skriptitiedostoon ja aja se komentoriviltä:

```Bash
#!/bin/bash

date_string=$(date +"%Y-%m-%d %H:%M:%S")

echo "Päivämäärä ja aika ovat nyt: $date_string"
```

Kun aja skripti, näet seuraavan tulosteen:

```
Päivämäärä ja aika ovat nyt: 2021-09-13 13:45:21
```

Tämä esimerkki käyttää `date` -komennon`+%Y-%m-%d %H:%M:%S` -parametria, joka määrittelee halutun tekstimuodon. Voit muuttaa tekstiä muuttamalla tätä parametria. Esimerkiksi jos haluat tekstin muodossa "13.09.2021 klo 13:45:21", voit käyttää parametria `"+%d.%m.%Y klo %H:%M:%S"`.

## Syväsukellus

Tässä esimerkissä käytämme `%Y-%m-%d %H:%M:%S` -parametria, joka määrittelee täysin muotoillun tekstin vuosiluvun, kuukauden, päivämäärän, tunnin, minuutin ja sekunnin osalta. Bash tarjoaa muitakin vaihtoehtoja, kuten `%d` (päivämäärä kahdella numerolla), `%b` (kuukauden nimi lyhyenä) ja `%a` (viikonpäivän nimi lyhyenä). Voit löytää lisää vaihtoehtoja Bashin virallisesta dokumentaatiosta.

## Katso myös

- [Bash - virallinen dokumentaatio](https://www.gnu.org/software/bash/manual/bash.html)
- [Bashin päivämäärä ja-aika -blogikirjoitus (englanniksi)](https://www.linuxjournal.com/content/converting-date-string-bash)