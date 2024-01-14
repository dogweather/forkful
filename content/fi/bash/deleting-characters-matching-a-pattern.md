---
title:                "Bash: Mallin mukaisten merkkien poistaminen"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit poistaa merkkejä, jotka vastaavat tiettyä mallia? Vaikka tämä voi tuntua perusteettomalta tai turhalta koodilta, se voi olla itse asiassa hyödyllinen taito, joka auttaa sinua tehokkaammin käsittelemään tekstidataa. Esimerkiksi voit käyttää tätä tekniikkaa poistaaksesi kaikki ylimääräiset välilyönnit tekstimuotoilua vaativista tiedostoista.

## Kuinka

Bash-skriptissä voit käyttää yksinkertaista komentoa nimeltä "sed" poistaaksesi merkkejä, jotka vastaavat tiettyä mallia. Seuraavassa esimerkissä käytämme sed-komentoja poistaaksesi kaikki numeroja sisältävät merkit ja jättääksemme jäljelle vain sanat.

```Bash
sed 's/[0-9]//g' tiedosto.txt
```

Input:

```Bash
Tämä on esimerkkilause, jossa on muutama numero, kuten 123.
```

Output:

```Bash
Tämä on esimerkkilause, jossa on muutama numero, kuten .
```

Seuraavassa esimerkissä käytämme sed-komentoja poistaaksemme kaikki kaksoispiste -merkit ja korvaamme ne yhdellä välilyönnillä.

```Bash
sed 's/:/ /g' tiedosto.txt
```

Input:

```Bash
Nimi:Sukunimi:Ika
```

Output:

```Bash
Nimi Sukunimi Ika
```

## Syvällinen tarkastelu

Sed-komento käyttää säännöllisiä lausekkeita (regex), jotka määrittävät, mitä merkkejä ja millä tavalla tiedostossa muutetaan. Säännölliset lausekkeet voivat olla hyödyllisiä monimutkaisempien hankkeiden yhteydessä, ja ne tarjoavat paljon monipuolisempia muokkausmahdollisuuksia kuin yksinkertainen merkkijonon vaihto. Voit käyttää myös muita Bash-komentoja, kuten "tr", "awk" ja "grep", poistaaksesi merkkejä. On hyvä harjoitella näiden komentojen käyttöä ja tarkastella niiden erilaisia toimintoja.

## Katso myös

- [SED-komento GNU-projektin verkkosivuilla](https://www.gnu.org/software/sed/)
- [Bash-skriptausopas](https://linux.die.net/abs-guide/)
- [Säännölliset lausekkeet 101](https://www.regular-expressions.info/)