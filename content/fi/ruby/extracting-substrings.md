---
title:    "Ruby: Alituksen erottaminen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: Miksi  joku haluaisi erottaa osajonoja

Substringien erottaminen on tärkeä osa Ruby-ohjelmoinnissa, koska se mahdollistaa tekstien ja merkkijonojen tehokkaan käsittelyn. Se voi myös auttaa parantamaan koodin suorituskykyä ja lisäämään toiminnallisuutta.

## Miten tehdä se: Koodin esimerkkejä ja tulosteita käyttäen "```Ruby ... ```" koodilohkoja.

```Ruby
# Luo muuttuja, jossa on merkkijono
teksti = "Tervetuloa Rubyyn!"

# Etsi ja tulosta tekstistä alkuosasta merkkijono
puts teksti[0,7] # Tulostaa "Tervetu"
puts teksti[8,2] # Tulostaa "lo"

# Etsi ja tulosta tekstistä loppuosasta merkkijono
puts teksti[-6,5] # Tulostaa "Ruby"
puts teksti[-4,4] # Tulostaa "yn!"

# Voit myös yhdistää merkkijonoja ja tulostaa ne samalla koodirivillä
puts teksti[8,2] + teksti[-4,4] # Tulostaa "loyn!"

# Käytä replace-metodia korvaamaan merkkijonoja
puts teksti.replace("Tervehdys maailmalle!") # Tulostaa "Tervehdys maailmalle!"
```

## Syvemmältä tutkien

Rubyssa substringeja voi erottaa käyttämällä indeksejä merkkijonon alusta tai lopusta. Voit määrittää indeksin käyttämällä hakasuluja merkkijonon perässä, ja määrittää halutun merkkijonon pituuden käyttämällä pilkkua. Jos haluat käyttää kokonaista merkkijonoa, voit käyttää "..." sijasta ".." määrittäessäsi sijainnin.

Substringien erottaminen voi myös auttaa suorituskyvyn parantamisessa, koska merkkijonan alku- tai loppuosa voidaan vaihtaa nopeammin kuin koko merkkijono. Se voi myös auttaa lisäämään toiminnallisuutta, koska voit käyttää replace-metodia korvaamaan merkkijonoja haluamillasi teksteillä.

## Katso myös
- Ruby:n viralliset dokumentaatiot substringeista: https://ruby-doc.org/core-2.7.1/String.html#method-i-5B-5D
- Selitys merkki-indeksistä Rubyssa: https://www.tutorialspoint.com/ruby/ruby_strings.htm