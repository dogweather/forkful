---
title:    "Ruby: Merkkijonojen yhdistäminen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Miksi yhdistää merkkijonoja? Siihen on monia syitä. Yksi tärkeimmistä syistä on, että yhdistetyt merkkijonot tarjoavat meille joustavuutta ja mahdollisuuden luoda kompleksisia lauseita ja tekstejä. Se myös auttaa meitä ylläpitämään siistiä ja helposti luettavaa koodia.

## Miten

Yhdistäminen merkkijonoja Rubyssa on yksinkertaista. Voit käyttää plus-merkkiä (+) tai concat-metodia. Katsotaanpa muutamia esimerkkejä:

```Ruby
str1 = "Hei"
str2 = "maailma"
str3 = str1 + " " + str2
puts str3
```

Tulostus: "Hei maailma"

```Ruby
name = "Sara"
greeting = "Moi"
sentence = greeting.concat(" ", name, "!")
puts sentence
```

Tulostus: "Moi Sara!"

Huomaa, että plus-merkki (+) myös toimii numeroiden yhdistämiseen.

## Syvällinen sukellus

Yhdistämisen lisäksi, Ruby tarjoaa myös monia muita hyödyllisiä merkkijonojen muokkausmenetelmiä. Yksi tärkeimmistä on interpolointi, joka mahdollistaa muuttujien tai lausekkeiden sisällyttämisen merkkijonoon. Voit tehdä tämän käyttämällä #-merkkiä ja aaltosulkia ({ }), kuten tässä esimerkissä:

```Ruby
name = "Matias"
sentence = "Tervehdys, #{name}! Kuinka voit?"
puts sentence
```

Tulostus: "Tervehdys, Matias! Kuinka voit?"

Lisäksi Rubyssa on myös monia muita hyödyllisiä merkkijonojen muokkausmenetelmiä, kuten upcase, downcase ja capitalize. Näitä voit käyttää muuntaaksesi merkkijonoja haluamallasi tavalla.

## Katso myös

* Ruby:n merkkijonojen interpolointi: https://ruby-doc.org/core-2.7.1/doc/syntax/literals_rdoc.html#label-String+Interpolation
* Muut hyödylliset merkkijonomuokkausmenetelmät Rubyssa: https://ruby-doc.org/core-2.7.1/String.html