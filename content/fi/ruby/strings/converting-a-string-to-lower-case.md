---
date: 2024-01-20 17:39:01.554231-07:00
description: "How to: String-luokan `downcase`-metodi on ollut Rubyn core-kirjastossa\
  \ alusta asti. Se muuttaa merkkijonon jokaisen kirjaimen vastaavaksi\u2026"
lastmod: '2024-04-05T21:53:58.655972-06:00'
model: gpt-4-1106-preview
summary: String-luokan `downcase`-metodi on ollut Rubyn core-kirjastossa alusta asti.
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
weight: 4
---

## How to:
```Ruby
# Ennen alkuun pääsyä - varmista, että olet asentanut uusimman version Rubysta.
# String-olioon pienennys Rubyssa:
esimerkki = "Tämä On Ruby Esimerkki!"
puts esimerkki.downcase
# Tulostaa: "tämä on ruby esimerkki!"
```

## Deep Dive:
String-luokan `downcase`-metodi on ollut Rubyn core-kirjastossa alusta asti. Se muuttaa merkkijonon jokaisen kirjaimen vastaavaksi pienikirjaimiseksi. Historiallisesti tämä on ollut tärkeää, koska tietokoneet erottavat isot ja pienet kirjaimet. Vaihtoehtoina on myös `downcase!`, joka muuttaa alkuperäisen merkkijonon paikan päällä, ja Railsin `squish`, joka poistaa ylimääräiset välilyönnit ja tekee `downcase`. Implementaation yksityiskohdat liittyvät siihen, miten Ruby käsittelee merkkien encodingia, jotta metodi toimii kaikilla eri merkistöillä.

## See Also:
- Ruby-dokumentaatio String#downcase: [ruby-doc.org](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase)
- Rubyn metaprogrointia ja sen metodeja käsittelevä opas: [https://poignant.guide](https://poignant.guide)
- Ruby Style Guide, jonka avulla koodisi pysyy puhtaana ja ylläpidettävänä: [https://rubystyle.guide](https://rubystyle.guide)
