---
title:                "Merkkijonon alkukirjainten suuriksi muuttaminen"
html_title:           "Gleam: Merkkijonon alkukirjainten suuriksi muuttaminen"
simple_title:         "Merkkijonon alkukirjainten suuriksi muuttaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisimme vaihtaa merkkijonon ensimmäisen kirjaimen isoon alkukirjaimeen? Tämä yksinkertainen muutos voi lisätä merkkijonon luettavuutta ja selkeyttä ohjelmassa.

## Kuinka tehdä

Käytämme Gleam-ohjelmointikieltä tehdäksemme tämän muutoksen. Kirjoitamme funktion nimeltä "capitalize", joka ottaa parametrinaan merkkijonon. Sitten käytämme "String.slice" -funktiota jakamaan merkkijonon kahteen osaan: ensimmäinen kirjain ja loput merkkijonosta. Käytämme sitten "String.to_upper_case" -funktiota muuttaaksemme ensimmäisen kirjaimen isoon alkukirjaimeen ja liitämme sen yhteen lopputuloksen kanssa. Lopuksi, palautamme uuden merkkijonon, jossa ensimmäinen kirjain on vaihdettu isoon alkukirjaimeen.

```
Gleam. capitalize (str) ->
  let first = String.slice(str, 0, 1)
  let rest = String.slice(str, 1, String.length(str))
  String.to_upper_case(first) ++ rest
```

Kutsuessaan "capitalize" -funktiota parametrina "hello", se palauttaisi "Hello" merkkijonon.

## Syvemmälle

Olet ehkä huomannut, että käytimme "++" -merkkiä lisätäksemme kaksi merkkijonoa yhteen. Tämä on osa Gleamin "String" -moduulia ja se yhdistää kaksi merkkijonoa yhteen uudeksi merkkijonoksi. Toinen hyödyllinen funktio, jonka käytimme, on "String.length", joka palauttaa merkkijonon pituuden. Näiden kahden funktion avulla pystymme leikkaamaan merkkijonoja haluamallamme tavalla.

## Katso myös

- [String Moduuli Gleamin Dokumentaatiosta](https://gleam.run/core/string.html)
- [Ohjevideo String Moduulista Gleamilla](https://www.youtube.com/watch?v=MXAaIVT5rbY)
- [Käytä Syötetiedostotyyppiä Gleamilla](https://medium.com/@louismeunier/using-input-output-types-in-gleam-be696f7c5164)