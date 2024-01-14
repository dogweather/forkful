---
title:                "Fish Shell: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoidessa voi tuntua turhauttavalta, kun ohjelman suorittamisen aikana tulee vastaan virheitä tai muita ilmoituksia. Yksi tapa hallita näitä ilmoituksia on kirjoittamalla niitä standardivirheen ulostuloon (standard error). Tämä auttaa sinua tunnistamaan ja korjaamaan ohjelman ongelmakohdat tehokkaammin.

## Miten tehdä

*Käytä Fish Shellin `echo` komentoa kirjoittaaksesi tekstin suoraan standardivirheeseen:*

```Fish Shell
echo "Tämä on virhe!" >&2
```

*Kun haluat ohjelman tuottavan sekä standardivirhettä että standarditulostusta, käytä `2>&1` reitityskomentoa:*

```Fish Shell
ls -invalid 2>&1
```

*Tämä yhdistää standardivirheen ja -tulostuksen ja luo yhden yhdistetyn virheilmoituksen.*

## Syvempi sukellus

Standardivirhe on yksi kolmesta tiedostovirrasta, joita käytetään kommunikoimaan ohjelman ja käyttöjärjestelmän välillä. Se on yleensä numeroitu *2* ja ohjelmassa käytetään redirection-komentoja reitittämään se halutulle kanavalle.

Kun tämä virheilmoitus tallennetaan tiedostoon, siitä tulee jälkihoitojälkeä. Voit myös käyttää sitä apuna virheiden löytämisessä ohjelman suorituksen jälkeen.

## Katso myös

- [Fish Shellin dokumentaatio](https://fishshell.com/docs/current/)
- [Standardi virhevienti-ohjeet UNIX-järjestelmissä](https://www.csee.umbc.edu/courses/331/resources/tutorials/unix_fundamentals/part12.html)
- [Linux Komennon opas](https://linuxcommand.org/index.php)