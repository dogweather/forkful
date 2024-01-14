---
title:                "Fish Shell: Päiväys muuttaminen merkkijonoksi"
simple_title:         "Päiväys muuttaminen merkkijonoksi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointiprojekteissa saattaa olla tarve muuttaa päivämäärä merkkijonoksi. Tämä on hyödyllistä esimerkiksi silloin, kun halutaan tulostaa päivämäärä tietokannan kyselyyn tai esittää se käyttäjälle ymmärrettävässä muodossa. Fish Shell tarjoaa kätevän tavan muuntaa päivämäärä merkkijonoksi.

## Kuinka tehdä

Fish Shellin ```date``` komennolla voidaan muuttaa päivämäärä halutunlaiseen merkkijonoon. Seuraavassa esimerkissä tulostetaan päivämäärä tänään ja huomenna ISO-muodossa:

```
date -I
2021-09-09
date -I -d '+1 day'
2021-09-10
```

Deep Dive: Päivämäärän muuntaminen ei ole aina yksinkertaista, sillä monissa eri maissa ja kulttuureissa käytetään erilaisia päivämäärän esitysmuotoja. Lisäksi päivämäärään liittyvät erilaiset aikavyöhykkeet ja kesäaika voivat aiheuttaa haasteita. Fish Shell tarjoaa kuitenkin laajan valikoiman vaihtoehtoja ja säädettävyyttä, mikä tekee päivämäärän muuntamisesta joustavaa ja helppoa.

## Katso myös

- [Fish Shell](https://fishshell.com/)
- [Fish Shell - dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHub-sivu](https://github.com/fish-shell/fish-shell)