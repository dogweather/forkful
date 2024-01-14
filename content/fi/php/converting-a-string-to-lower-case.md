---
title:                "PHP: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Tässä blogipostauksessa käsittelemme, miksi ja miten muuttaa merkkijono pieniksi kirjaimiksi käyttämällä PHP-ohjelmointikieltä. Pieniä kirjaimia käytetään laajasti ohjelmoinnissa, ja tämän taidon hallinta auttaa välttämään virheitä ja tekee koodistasi helpommin luettavaa ja ymmärrettävää.

## Miten

Merkkijonon muuttaminen pieniksi kirjaimiksi PHP:ssa on helppoa. Voit käyttää strtolower-funktiota, joka ottaa merkkijonon ja palauttaa saman merkkijonon, jossa kaikki kirjaimet on muutettu pieniksi. Alla on esimerkki:

```PHP
$merkkijono = "TÄMÄ ON MERKKIJONO";
echo strtolower($merkkijono);
```
Tulostus:
`tämä on merkkijono`

Voit myös muuttaa merkkijonon pieniksi kirjaimiksi käyttämällä strtoupper-funktiota, joka tekee päinvastaisen eli muuttaa kaikki kirjaimet isoiksi.

```PHP
$merkkijono = "tämä on merkkijono";
echo strtoupper($merkkijono);
```
Tulostus:
`TÄMÄ ON MERKKIJONO`

On myös mahdollista muuttaa merkkijonossa olevia kirjaimia, joihin ei liity kielitieteellisiä sääntöjä. Tämä voidaan tehdä käyttämällä mb_strtoupper- ja mb_strtolower-funktioita.

## Syvempää sukellusta

Älä unohda, että PHP on C-kielipohjainen ja kirjaimien muuntaminen suoritetaan ASCII-yhteensopivilla menetelmillä. Tämä tarkoittaa sitä, että merkkijonojen muuntamisessa käytetään koko merkkikoodisarjaa eikä vain kirjaimia, joita käytetään puhekielessä.

Esimerkiksi ä-kirjain voi olla joko kirjaimella 'a' tai kaksi merkkiä 'aa', jotka muodostavat yhden kirjaimen. Samoin kirjain 'Ö' voidaan muodostaa sekä yhdestä että kahdesta merkistä 'öö'.

### Kirjainlajit

PHP tukee monia erilaisia kirjainlajeja. Jokainen kirjainlaji käy läpi erilaisia käsittelyjä, kuten pienen ja ison kirjaimen muutokset tai kirjainten taitava suodattaminen. Voit lisätä lisää kirjainlajeja käyttämällä stringiä, joka sisältää kaikki tarvittavat lajit.

Kokonaisuudessaan nämä kirjainlajit edustavat isoja ja pieniä latinalaisia kirjaimia, kreikkalaisia ja kyrillisiä kirjaimia sekä pisteitä tai diakriittisiä merkkejä. Voit helposti lisätä ja määrittää omat kirjainlajisi käyttämällä ASCII-yhteensopivia merkkialueita.

## Katso myös

- [PHP:n merkkijonojen manuaalisivu](https://www.php.net/manual/en/features.string-case.php)
- [ASCII-merkkikoodisarja](https://www.ascii-code.com/)