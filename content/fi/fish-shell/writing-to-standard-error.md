---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "Fish Shell: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Mitä & Miksi?

Kun ohjelmoijat käyttävät Fish Shell -ohjelmointikieltä, he usein kirjoittavat virheitä standardivirheeseen. Tämä tarkoittaa käytännössä sitä, että virheet ja ilmoitukset näkyvät vain terminaalissa eivätkä sekoita normaaliin tulostukseen. Tämä auttaa ohjelmoijia havaitsemaan ja korjaamaan virheitä nopeammin ja tehokkaammin.

## Kuinka:

Fish Shell -ohjelmointikieli tarjoaa helpon ja yksinkertaisen tavan kirjoittaa virheitä standardivirheeseen. Käytä komentoa `echo` ja lisää `-e` -lippu, jotta voit käyttää erikoismerkkejä, kuten uudet rivit ja välilyönnit, kuten esimerkissä alla. Huomaa, että virheellinen rivien järjestys johtaa virheelliseen tulostukseen.

```
Fish Shell koodi:
echo -e "Tämä on ensimmäinen rivi. \nTämä on toinen rivi."
```
```
Tulostus:
Tämä on ensimmäinen rivi.
Tämä on toinen rivi.
```

## Syvempää tietoa:

Historiallisesti ohjelmoijat ovat kirjoittaneet virheitä standardivirheeseen, jotta ne eivät sekoitu normaaliin tulostukseen. Tämä käytäntö on edelleen suosittu ja käytössä monissa muissa ohjelmointikielissä, kuten Bash ja Python.

On myös muita tapoja kirjoittaa virheitä standardivirheeseen Fish Shellissä, kuten komennolla `print -e`, mutta `echo` on yleisimmin käytetty tapa.

## Katso myös:

- Fish Shellin viralliset dokumentaatiot: https://fishshell.com/docs/current/
- Fish Shellin GitHub-sivusto: https://github.com/fish-shell/fish-shell
- "Kuinka käyttää Fish Shell -täydennysliitännäisiä": https://www.solidsmack.com/cad-design-news/fish-shell-plugins/