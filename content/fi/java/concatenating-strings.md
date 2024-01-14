---
title:                "Java: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/concatenating-strings.md"
---

{{< edit_this_page >}}

# Miksi: Miksi yhdistää merkkijonoja Java-ohjelmoinnissa?

## Miksi 

Merkkijonon yhdistäminen on yleinen tehtävä Java-ohjelmoinnissa. Se on hyödyllinen, kun halutaan yhdistää useita merkkijonoja yhdeksi tai kun halutaan lisätä muuttujan arvo merkkijonoon. Esimerkiksi, kun halutaan luoda lause, jossa käytetään muuttujan arvoa.

## Miten 

Merkkijonojen yhdistäminen onnistuu käyttämällä + -operaattoria. Se yhdistää kaksi merkkijonoa ja palauttaa uuden merkkijonon. Katsotaan esimerkkiä: 

```java
String etunimi = "Matti";
String sukunimi = "Meikäläinen";
String nimi = etunimi + " " + sukunimi;
System.out.println(nimi);
```

Tämä koodi tulostaisi "Matti Meikäläinen". Käytämme + -operaattoria yhdistämään etunimen, välilyönnin ja sukunimen yhdeksi merkkijonoksi. 

## Syväsukellus 

Merkkijonojen yhdistäminen käyttäen + -operaattoria on kätevä tapa yhdistää useita merkkijonoja yhdeksi. On kuitenkin tärkeää muistaa, että tämä ei muuta alkuperäisiä merkkijonoja, vaan luo uuden. 

On myös muita tapoja yhdistää merkkijonoja Java-ohjelmoinnissa. Voit esimerkiksi käyttää String.format()-metodia tai StringBuilder-oliota. Näitä kannattaa tutkia, jos haluat syventää ymmärrystäsi merkkijonojen yhdistämisestä. 

# Katso myös 

- [String.format() Java-opetusohjelma](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [StringBuilder Java-opetusohjelma](https://docs.oracle.com/javase/tutorial/java/data/buffers.html)