---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Joskus on tarpeen käsitellä verkkosivuilta saatavaa tietoa, esimerkiksi kaavioita, taulukoita tai tekstiä. Parsimalla HTML-koodia Bashilla voidaan helposti saada haluttu tieto ja käyttää sitä ohjelmointitehtävissä.

## Kuinka tehdä

Parsiminen Bashilla on helppoa, käytetään vain ```curl```-komennon avulla saatu HTML-koodi ```grep```, ```sed``` ja ```awk``` komentojen avulla. Alla on esimerkki HTML-dokumentin parsimisesta ja halutun tiedon tulostamisesta. 

```Bash
# Ensin ladataan HTML-dokumentti
curl https://www.example.com/ > example.html
# Sitten etsitään haluttu elementti html-tiedostosta ja tallennetaan se muuttujaan
title=$(grep "<title>" example.html | sed "s/<title>//g" | awk -F "</title>" '{print $1}')
# Lopuksi tulostetaan haluttu tieto
echo "$title"
```

Esimerkki tulostaa HTML-dokumentin otsikon ja tallentaa sen muuttujaan ```title```.

## Syvällinen tarkastelu

Parsiminen Bashilla perustuu eri komentojen yhdistämiseen HTML-dokumentin rivien läpi etsimällä haluttuja elementtejä ja muokkaamalla niitä tarvittaessa. Tämä mahdollistaa tarkan ja tietyn tiedon etsimisen ja käytön. Kannattaa tutustua myös muihin Bashin komentoihin, kuten ```cut``` ja ```grep -o```, joilla voi helpottaa tietyn tiedon etsimistä HTML-dokumentista.

## Katso myös

- [Bashin virallinen dokumentaatio](https://www.gnu.org/software/bash/)
- [Bashin opas - Tero Karvinen](http://terokarvinen.com/2019/aikataulu-palvelinten-hallinta-ict4tn022-5-autumn-2019/)