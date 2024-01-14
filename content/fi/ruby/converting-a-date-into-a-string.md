---
title:    "Ruby: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi muuntaa päivämäärän merkkijonoksi? Pelinä tehokkaasta ratkaisusta ja tarkoituksenmukaisesta ohjelmointikielestä, Rubyssa on useita käteviä tapoja käsitellä päivämääriä ja aikaa. Yksi näistä on päivämäärän muuntaminen merkkijonoksi, mikä voi olla hyödyllistä esimerkiksi lomakkeiden lähetys- ja tallennusprosessissa tai käyttäjien profiilisivuilla.

## Kuinka

Seuraavassa on kaksi esimerkkiä siitä, kuinka voit muuntaa päivämäärän merkkijonoksi Rubyssa:

```Ruby
date = DateTime.now
puts date.to_s # tulostaa päivämäärän ja ajan merkkijonona
```

```Ruby
date = Time.now
puts date.strftime("%d.%m.%Y") # tulostaa päivämäärän muodossa DD.MM.YYYY
```

Molemmat esimerkit hyödyntävät Rubyssa valmiiksi sisäänrakennettuja metodeja päivämäärän muuntamiseen merkkijonoksi. Ensimmäisessä esimerkissä käytämme `to_s` -metodia, joka palauttaa päivämäärän ja ajan merkkijonona. Toisessa esimerkissä käytämme `strftime`-metodia, joka antaa meille enemmän hallintaa siitä, millaisessa muodossa haluamme päivämäärän tulostettavan.

## Syventävä sukellus

Mikäli haluat syvemmän ymmärryksen päivämäärän muuntamisesta merkkijonoksi Rubyssa, on hyödyllistä tietää, että Ruby sisältää myös `Date` ja `Time` -luokat, jotka tarjoavat erilaisia metodeja päivämäärän käsittelyyn. Näiden luokkien dokumentaatio on hyvä paikka aloittaa, mikäli olet kiinnostunut oppimaan lisää niiden tarjoamista toiminnoista.

Lisäksi on hyvä huomioida, että päivämäärän muuntaminen merkkijonoksi voi olla myös haastavaa, mikäli päivämäärätiedot eivät ole oikeassa muodossa. On siis tärkeää varmistaa, että käsiteltävät päivämääräarvot ovat oikeassa formaatissa ennen muunnosta.

## Katso myös

- [Ruby `Date` Class Documentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [Ruby `Time` Class Documentation](https://ruby-doc.org/core-2.7.0/Time.html)
- [`to_s` vs. `strftime` in Ruby](https://stackoverflow.com/questions/29275736/difference-between-to-s-and-strftime)