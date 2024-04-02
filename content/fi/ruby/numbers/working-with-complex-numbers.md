---
date: 2024-01-26 04:45:14.499712-07:00
description: "Kompleksiluvut, jotka koostuvat reaali- ja imaginaariosasta (kuten 3+4i),\
  \ ovat vakiovaruste insin\xF6\xF6reill\xE4 ja fyysikoilla. Ohjelmoijat ty\xF6skentelev\xE4\
  t\u2026"
lastmod: '2024-03-13T22:44:57.078711-06:00'
model: gpt-4-0125-preview
summary: "Kompleksiluvut, jotka koostuvat reaali- ja imaginaariosasta (kuten 3+4i),\
  \ ovat vakiovaruste insin\xF6\xF6reill\xE4 ja fyysikoilla. Ohjelmoijat ty\xF6skentelev\xE4\
  t\u2026"
title: "Kompleksilukujen k\xE4sittely"
weight: 14
---

## Mikä & Miksi?
Kompleksiluvut, jotka koostuvat reaali- ja imaginaariosasta (kuten 3+4i), ovat vakiovaruste insinööreillä ja fyysikoilla. Ohjelmoijat työskentelevät niiden kanssa simulaatioissa, signaalinkäsittelyssä ja yhtälöiden ratkaisemisessa, jotka eivät toimi pelkästään reaalilukujen kanssa.

## Miten:
Ruby tekee kompleksilukujen käsittelystä tuulen. Voit luoda ja manipuloida niitä Complex-luokan avulla:

```ruby
require 'complex'

# Luo kompleksilukuja
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# Perusoperaatiot
summa = c1 + c2               # => (5.0+9.0i)
ero = c1 - c2                 # => (1.0-1.0i)
tulo = c1 * c2                # => (-14.0+23.0i)
osamäärä = c1 / c2            # => (0.896551724137931+0.03448275862068961i)

# Konjugaatti, magnitudi ja vaihe
konjugaatti = c1.conjugate    # => (3.0-4.0i)
magnitudi = c1.abs            # => 5.0
vaihe = c1.phase              # Math.atan2(4, 3) => 0.9272952180016122 radiaania

# Kompleksilukuihin erikoistuneet metodit
polaarinen = c1.polar         # => [5.0, 0.9272952180016122]
suorakulmainen = c1.rect      # => [3.0, 4.0]
```

## Syväsukellus
Kompleksiluvut eivät ole uusia – niitä on ollut jo 1500-luvulta lähtien ratkaisemassa yhtälöitä, joilla ei ole reaalilukuratkaisuja. Matematiikan ohella, laskennallisesti Ruby:n Complex-luokka tekee raskaan työn, jota Math-moduuli tukee trigonometrisillä ja transsendenttisilla funktioilla.

Aikaisemmissa ohjelmointikielissä reaali- ja imaginaariosien käsittely vaati manuaalista työtä. Jotkut, kuten Fortran ja C++, omistavat erityisiä kirjastoja kompleksiaritmetiikalle.

Rubyn lähestymistapa sisältää kompleksilukujen tuen sen syntaksissa, vapauttaen sinut keksimästä pyörää uudelleen. Taustalla Complex-luokka käsittelee matematiikan, kun taas Ruby huolehtii objektivuorovaikutuksista.

## Katso Myös
- Rubyn dokumentaatio Complexista: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- MathWorldin näkemys kompleksiluvuista: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- Visuaalinen johdatus kompleksilukuihin ja miksi ne ovat hyödyllisiä: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
