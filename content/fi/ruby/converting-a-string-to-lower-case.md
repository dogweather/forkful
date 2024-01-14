---
title:    "Ruby: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Monien ohjelmointikielien joukossa Ruby erottuu helppoutensa ja luettavuutensa ansiosta. Yksi sen kätevistä ominaisuuksista on kyky muuttaa merkkijono pienikirjaimiseksi. Tässä blogikirjoituksessa käymme läpi, miksi voit haluta tehdä tämän ja miten se tehdään.

## Miten

Koodiesimerkkejä ja tulosteita näet alla olevissa "```Ruby ... ```" -koodilohkoissa.

```Ruby
string = "TÄMÄ ON TEKSTIÄ"
puts string.downcase
```

Tuloste: "tämä on tekstiä"

```Ruby
another_string = "Oletko Valmis ???"
puts another_string.downcase
```

Tuloste: "oletko valmis ???"

## Syvällisempi katsaus

Kun muutat merkkijono pienikirjaimiseksi, Ruby käyttää sisäistä ```downcase``` -metodia. Se muuttaa myös diakriittiset merkit oikein, kuten esimerkiksi ääkköset ja Š-merkit.

Lisäksi on hyvä muistaa, että ```downcase``` -metodi ei muuta alkuperäistä merkkijonoa, vaan palauttaa uuden merkkijonon pienikirjaimisena. Tämä on tärkeää ottaa huomioon, jos haluat tallentaa muutetun merkkijonon uuteen muuttujaan.

## Katso myös

- [Ruby: Merkkijonon muuttaminen pienikirjaimiseksi](https://www.ruby-lang.org/fi/documentation/ruby-from-other-languages/to-ruby-language/string/)
- [Ruby: Diakriittisten merkkien käsittely](https://www.ruby-lang.org/fi/documentation/syntax/string/#special_characters)
- [Wikipedia: Diakriittiset merkit](https://fi.wikipedia.org/wiki/Luettelo_suomenkielisist%C3%A4_diakriittisist%C3%A4_merkeist%C3%A4)