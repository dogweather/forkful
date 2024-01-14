---
title:                "Ruby: Merkkijonon suurennus"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Käytettäessä ohjelmointikieltä Ruby, saatat jossain vaiheessa törmätä tarpeeseen muuttaa merkkijono, eli string, isoiksi kirjaimiksi. Tässä blogikirjoituksessa kerromme, miksi tähän voisi olla tarvetta ja miten se tehdään.

## Miten tehdä

Merkkijonon isoiksi kirjaimiksi muuttamiseen on Rubyssa käytettävissä valmis metodi nimeltään .capitalize. Seuraavassa esimerkissä käytämme sitä yhdessä muuttujan kanssa:

```Ruby
nimi = "mikko"
p nimi.capitalize
```
Tulosteena saamme "Mikko", jossa nimen alkukirjain on muuttunut isoksi.

Toinen tapa muuttaa merkkijono isoihin kirjaimiin on käyttää .upcase-metodia. Se muuttaa kaikki merkit isoksi kirjaimiksi, eikä pelkästään alkukirjainta:

```Ruby
nimi = "mikko"
p nimi.upcase
```
Tulosteena saamme "MIKKO".

## Syvällinen sukellus

Rubyssa on monia eri tapoja muuttaa merkkijonon kuin merkkijonon isoksi. Esimerkiksi .capitalize!-metodi muuttaa muuttujan arvon pysyvästi isoksi kirjaimeksi. On myös mahdollista käyttää Regexiä, eli säännöllisiä lausekkeita, merkkijonon muuttamiseen isoksi.

## Katso myös

- https://www.rubyguides.com/2019/04/ruby-string-methods/
- https://apidock.com/ruby/String/capitalize
- http://ruby-doc.org/core-2.6.3/String.html#method-i-capitalize