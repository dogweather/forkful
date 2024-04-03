---
date: 2024-01-26 03:46:40.954767-07:00
description: 'Miten: .'
lastmod: '2024-03-13T22:44:57.079629-06:00'
model: gpt-4-0125-preview
summary: .
title: "Numerojen py\xF6rist\xE4minen"
weight: 13
---

## Miten:
```Ruby
# Peruspyöristäminen
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# Tarkkuuden määrittäminen
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# Pyöristäminen alas
puts 2.9.floor          # => 2

# Pyöristäminen ylös
puts 2.1.ceil           # => 3

# Pyöristäminen kohti nollaa
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

Esimerkkitulo:
```
3
3
3.14
2.68
2
3
-3
-2
```

## Syväsukellus
Lukujen pyöristäminen ei ole uutta — ihmisiä on tehty sen vuosisatojen ajan tehdäkseen laskelmista helpompia tai toimiakseen työkalujensa rajoissa. Rubyn `round`-metodi on monipuolinen, kyvyllään pyöristää lähimpään kokonaislukuun oletuksena tai määritettyyn desimaalipaikkaan.

Vaihtoehto `round`-metodille on `floor` aina pyöristettäessä alas, ja `ceil` aina pyöristettäessä ylös, riippumatta luvun arvosta. Desimaalipaikkojen poistamiseen on käytössä `truncate`.

Historiallisesti tietokoneiden kannalta pyöristäminen muuttuu kriittiseksi käsiteltäessä liukulukuaritmetiikkaa sen synnynnäisen epätarkkuuden vuoksi. Ruby, kuten useimmat kielet, noudattaa IEEE 754 -standardia liukuluvuille, mikä tarkoittaa, että se käsittelee pyöristämistä tavalla, johon useimmat ohjelmoijat pystyvät ennakoimaan ja luottamaan.

Mutta, siinä on enemmänkin—asiat kuten pankkiirin pyöristäminen (tunnetaan myös nimellä pyöristä puoliksi parilliseen) ovat käsitteitä, jotka Ruby-kehittäjien saattaa tarvita toteuttaa manuaalisesti, koska `round`-metodi ei tarjoa sitä suoraan.

## Katso Myös
- [Rubyn dokumentaatio](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round) liukulukujen `round`-metodista.
- [IEEE-standardi liukulukuaritmetiikalle (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- [Ymmärtäminen liukulukujen tarkkuudesta](https://floating-point-gui.de/), syvällisempi katsaus siihen, miten tietokoneet käsittelevät desimaalilukuja.
