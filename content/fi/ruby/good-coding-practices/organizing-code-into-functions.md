---
date: 2024-01-26 01:16:22.243342-07:00
description: "Kuinka: Kuvittele, ett\xE4 kirjoitat pikaskriptin k\xE4ytt\xE4jien tervehtimiseen."
lastmod: '2024-03-13T22:44:57.092416-06:00'
model: gpt-4-0125-preview
summary: "Kuvittele, ett\xE4 kirjoitat pikaskriptin k\xE4ytt\xE4jien tervehtimiseen."
title: "Koodin j\xE4rjest\xE4minen funktioihin"
weight: 18
---

## Kuinka:
Kuvittele, että kirjoitat pikaskriptin käyttäjien tervehtimiseen:

```Ruby
def tervehdi(nimi)
  "Hei, #{nimi}!"
end

puts tervehdi("Alice")   # Tuloste: Hei, Alice!
puts tervehdi("Bob")     # Tuloste: Hei, Bob!
```

Tai ehkä lasket ympyrän alaa:

```Ruby
def ympyran_ala(sade)
  Math::PI * sade ** 2
end

puts ympyran_ala(5)   # Tuloste: 78.53981633974483
```

Siistimpää ja helpommin käsiteltävää, eikö vain?

## Syväsukellus
Funktioiden, joita Rubyn kielessä kutsutaan myös metodeiksi, konsepti ei ole uusi – se on yhtä vanha kuin ohjelmointikin. Palataanpa takaisin 1950-luvulle, aliohjelmat, kuten niitä kutsuttiin, otettiin käyttöön toiston vähentämiseksi.

Vaihtoehtoja? Toki, sinulla on suoraa koodia, voit mennä OOP:hen luokkien ja olioiden kanssa, tai jopa funktionaaliseen ohjelmointiin lambdojen ja procien avulla. Mutta funktiot ovat järjestyneen koodin leipä ja voi. Haluatko suorituskykyä? Funktioiden paikalliset muuttujat ovat nopeita, ja funktiot voivat palauttaa arvot välittömästi `return`-komennon avulla.

Toteutuksen kannalta voit määritellä funktion `def`-avainsanalla ja päättää sen `end`-avainsanalla. Voit asettaa oletusparametreja, käyttää splat-operaattoreita variadisten funktioiden kanssa ja muuta. Funktiot voivat olla yksinkertaisia tai monimutkaisia, kuten sydämesi halajaa.

## Katso myös
- [Rubyn metodidokumentaatio](https://ruby-doc.org/core-2.7.0/Method.html)
- [Ohjelmoinnin opettelu Chris Pinen mukaan](https://pine.fm/LearnToProgram/)
- [Käytännöllinen objektiivinen suunnittelu Rubylla, Sandi Metzin kirjoittama](https://www.poodr.com/)
