---
date: 2024-01-26 04:17:18.189518-07:00
description: "Interaktiivinen kuori, tai REPL (Read-Eval-Print Loop, lue-arvioi-tulosta\
  \ -silmukka), mahdollistaa koodin testaamisen reaaliajassa. Ohjelmoijat k\xE4ytt\xE4\
  v\xE4t\u2026"
lastmod: '2024-03-13T22:44:57.088122-06:00'
model: gpt-4-0125-preview
summary: Interaktiivinen kuori, tai REPL (Read-Eval-Print Loop, lue-arvioi-tulosta
  -silmukka), mahdollistaa koodin testaamisen reaaliajassa.
title: "Interaktiivisen komentotulkin (REPL) k\xE4ytt\xF6"
weight: 34
---

## Kuinka:
Rubyn REPL on nimeltään IRB (Interactive Ruby). Hyppää sisään ja kokeile Rubyä suoraan terminaalistasi:

```Ruby
irb
2.7.0 :001 > puts "Hei, Ruby-maailma!"
Hei, Ruby-maailma!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## Syväsukellus
Ruby 1.8:ssa esitelty IRB on olennainen osa Ruby-käyttäjiä. Se on saanut inspiraationsa Lispin ja Pythonin interaktiivisista kuorista, yhdistäen kokeilut välittömään palautteeseen. Vaihtoehtoja, kuten Pry, tarjoavat lisäominaisuuksia kuten syntaksin korostuksen ja robustimman virheenkorjausympäristön. IRB sinänsä on yksinkertainen, mutta sen toiminnallisuutta voidaan laajentaa 'irbtools' -gemien avulla. IRB käsittelee lue-arvioi-tulosta -silmukan lukemalla jokaisen syötteen rivin, arvioimalla sen Ruby-koodina ja sitten tulostamalla tuloksen, toistaen tätä prosessia kunnes poistutaan.

## Katso myös
- [Rubyn IRB](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [Irbtools-gemi](https://github.com/janlelis/irbtools)
