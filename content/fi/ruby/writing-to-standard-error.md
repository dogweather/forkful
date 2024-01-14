---
title:                "Ruby: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standardivirheeseen voi tuntua turhalta askelta, sillä se ei näy lopullisessa ohjelmassa. Kuitenkin, jos haluat seurata tai havaita mahdollisia virheitä, niin kirjoittaminen standardivirheeseen on erittäin hyödyllinen tapa saada selville, mitä ohjelmassa tapahtuu.

## Miten

Seuraavassa esimerkissä näytämme, miten kirjoitat standardivirheeseen Ruby-ohjelmassa. Voit käyttää `puts`-metodia kirjoittamaan tekstiä standardilähtöön, mutta jos haluat kirjoittaa virheilmoituksia, käytä `STDERR.puts`-metodia.

```Ruby
begin
  # Tätä koodia yritetään suorittaa
  5 / 0
rescue
  # Jos virhe tapahtuu, kirjoitetaan virheilmoitus standardivirheeseen
  STDERR.puts "Virhe: Et voi jakaa nollalla."
end
```

Tämän ohjelman tulos näyttää seuraavalta:

```
Virhe: Et voi jakaa nollalla.
```

## Syvemmälle

Kun käytät `STDERR.puts`-metodia, voit myös antaa sille useita argumentteja. Tällöin ne tulostetaan standardivirheenä yksi kerrallaan.

```Ruby
# Kirjoitetaan standardivirheeseen useita viestejä
STDERR.puts "Tämä on ensimmäinen virhe."
STDERR.puts "Tämä on toinen virhe."
STDERR.puts "Tämä on kolmas virhe."
```

Tämän ohjelman tulos näyttää seuraavalta:

```
Tämä on ensimmäinen virhe.
Tämä on toinen virhe.
Tämä on kolmas virhe.
```

Voit myös käyttää `STDERR.print`-metodia, jos haluat tulostaa kaiken samalle riville. Tässä esimerkissä käytämme `STDERR.print`-metodia tulostamaan kymmenen peräkkäistä numeroa.

```Ruby
# Tulostetaan numerot 1-10 standardivirheeseen ilman rivinvaihtoa
(1..10).each do |i|
  STDERR.print "#{i} "
end
```

Tämän ohjelman tulos näyttää seuraavalta:

```
1 2 3 4 5 6 7 8 9 10
```

## Katso myös

- [STDERR luokka Ruby-ohjelmointikielen virallisessa dokumentaatiossa](https://ruby-doc.org/core-3.0.1/STDERR.html)
- [Ruby-tietokirja: Tulostaminen standardivirheeseen](http://rubylearning.com/satishtalim/writing-to-standard-error/)