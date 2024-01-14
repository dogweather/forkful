---
title:    "Ruby: Vianjäljitystulosteiden tulostaminen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Miksi tulostaa debugataustietoa?

Tulostamalla debugataustietoja voit tarkastella koodisi toimintaa tarkemmin ja selvittää mahdollisia virheitä koodissasi. Tämä on erittäin hyödyllistä, kun koodi ei toimi odotetusti tai haluat varmistaa, että kaikki muuttujat ja arvot muuttuvat oikein.

## Miten

Tässä esimerkissä tulostamme yksinkertaisen "Hello World" viestin Rubyllä käyttämällä ```puts```-metodia:

```Ruby
puts "Hello World!"
```

Tämä koodi tulostaa tekstin "Hello World!" terminaaliin, jolloin voit varmistaa, että koodi toimii oikein.

Voit myös tulostaa muuttujien arvoja käyttämällä ```puts```-metodia. Esimerkiksi:

```Ruby
x = 5
puts "Muuttujan x arvo on: #{x}"
```

Tämä tulostaa tekstin "Muuttujan x arvo on: 5" terminaaliin.

## Syvempi sukellus

Rubyllä on myös muita tapoja tulostaa debugataustietoja. Voit käyttää esimerkiksi ```p```-metodia, joka tulostaa kaiken tiedon halutusta muuttujasta tai objektista.

```Ruby
x = 5
p x
```

Tämä tulostaa muuttujan x kaiken tiedon, esimerkiksi sen tyyppin, arvon ja muut ominaisuudet.

Voit myös käyttää ```.inspect```-metodia tulostamaan muuttujan tai objektin tarkka sisältö. Esimerkiksi:

```Ruby
arvoja = [1, 2, 3]
arvoja.inspect
```

Tämä tulostaa kaikki arvot, jotka sisältyvät muuttujaan "arvoja".

## Katso myös

- [Ruby:n virallinen dokumentaatio](https://ruby-lang.org/en/documentation/)
- [Ruby:n debuggausvinkit](https://www.rubyguides.com/2016/10/ruby-debugging/)
- [Ruby Debugging Tools -opas](https://airbrake.io/blog/ruby-exception-handling/debugging-tools-ruby)