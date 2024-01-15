---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Ruby: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaa tiedostoon Ruby-kielellä? Tässä artikkelissa opit perusteet tekstitiedoston kirjoittamisesta ja saat samalla tärkeää tietoa Ruby-kielestä.

## Miten

Ruby-kielellä on helppo kirjoittaa tekstitiedostoja. Seuraa alla olevia esimerkkejä ja käytä koodinpätkät "```Ruby ... ```" määritelläksesi koodiesimerkkisi.

### Peruskomennon käyttö

```Ruby
File.write("tiedosto.txt", "Tämä on tekstiä")
```

Tämä koodi luo tai kirjoittaa tiedostoon "tiedosto.txt" tekstin "Tämä on tekstiä". Jos tiedosto on jo olemassa, se korvataan uudella tekstillä. Jos tiedostoa ei ole, se luodaan automaattisesti ja siihen kirjoitetaan teksti.

### Tiedostoon kirjoittaminen muuttujan avulla

```Ruby
nimi = "Mari"
File.write("tiedosto.txt", nimi)
```

Tämä esimerkki luo tai kirjoittaa tiedostoon "tiedosto.txt" muuttujan nimen arvon. Tiedostoon tallennetaan siis "Mari".

### Tiedostoon kirjoittaminen muotoilulla

```Ruby
teksti = "Tämä on tekstiä "
numerot = 123
File.write("tiedosto.txt", "#{teksti}ja #{numerot}")
```

Tässä esimerkissä kirjoitetaan tiedostoon "tiedosto.txt" teksti "Tämä on tekstiä ja 123". Käytössä on muotoilu, jossa käytetään #{}-merkintää muuttujien asettamiseen tekstiin.

## Syväkatsaus

Tekstitiedostojen kirjoittaminen on tärkeä osa ohjelmointia ja se tuo monia hyötyjä. Tekstitiedostoja voidaan käyttää esimerkiksi tietojen tallentamiseen tai backupsaamiseen, ja niitä voidaan myös lukea ja käsitellä myöhemmin esimerkiksi toisten ohjelmien kautta.

Ruby-kielellä on monipuoliset mahdollisuudet tiedostojen käsittelyyn ja sen syntax on helppo omaksua. Harjoittele kirjoittamista ja kokeile erilaisia vaihtoehtoja, niin opit hyödyntämään Ruby-kieen tekstitiedostoja monipuolisesti.

## Katso myös

- [Ruby-ohjelmointiopas](https://www.codecademy.com/learn/learn-ruby)
- [Official Ruby Documentation](https://www.ruby-lang.org/en/documentation/)
- [Rubyfor example](https://www.ruby-for-example.com/)