---
title:    "Ruby: Tiedoston lukeminen tekstimuodossa"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi sinun kannattaa lukea tiedostoa Ruby-kielen avulla? Kaksi sanaa: tiedonhallinta ja tehokkuus. Lue lisää nähdäksesi miten pystyt helposti lukemaan ja käsittelemään tiedostoja Ruby-ohjelmointikielellä.

## Kuinka

Seuraavassa esitellään joitakin esimerkkejä kuinka voit lukea tiedostoja Ruby-ohjelmointikielellä ja millainen on esimerkkilähtöluvun muotoilu Ruby-kielellä. Kuvatkaamme oletuksena, että haluat lukea tiedoston nimeltä "tilastot.csv". Voit käyttää `File.open` -funktiota luettavaksi `tilastot.csv` -tiedostoksi ja tallentaa sen muuttujaan, jotta voit käsitellä sitä:

```Ruby
tiedosto = File.open("tilastot.csv", "r")
```

Voit lukea tiedostosta yhden rivin kerrallaan käyttäen `gets` komentoa:

```Ruby
rivi = tiedosto.gets
```

Voit myös tulostaa kaiken sisällön käyttäen `readlines` -funktiota ja sen jälkeen käyttää `each` toimintoa käydäksesi läpi rivit ja tulostaaksesi ne:

```Ruby
sisalto = tiedosto.readlines

sisalto.each do |rivi|
  puts rivi
end
```

## Syvällinen sukellus

Kuten näet, Ruby-ohjelmointikieli käyttää `File.open` -funktiota tiedostojen lukemiseen sekä `gets` ja `readlines` -komentoja tiedostosta sisällön lukemiseen. On myös muita mahdollisia tapoja käsitellä tiedostoja Rubylla ja syvällisemmässä sukelluksessa voit tutkia niitä tarkemmin.

## Katso myös

Tässä on joitakin hyödyllisiä linkkejä jatkotutkimusta varten:

- [Ruby-dokumentaatio tiedostojen käsittelystä](https://ruby-doc.org/core-3.0.0/File.html)
- [Ruby-tiedostojen lukeminen ja kirjoittaminen](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)
- [Kurkkaus Ruby-kielen tiedostonkäsittelyyn](https://www.geeksforgeeks.org/ruby-file-i-o/)

Kiitos lukemisesta ja onnea tiedostojen käsittelyyn Rubylla!