---
title:    "Arduino: Alimerkkijonojen erottaminen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi

## Miksi haluat oppia tuottamaan alimerkkijonoja koodiasi varten? Alimerkkijonojen haku voi olla hyödyllistä, jos sinun täytyy käsitellä suurta määrää dataa ja haluat eristää tietyt osat siitä tai jos haluat muokata olemassa olevia merkkijonoja tietyllä tavalla.

## Kuinka tehdä

Alimerkkijonojen hakeminen Arduino-koodissa on suhteellisen helppoa, mutta vaatii hieman ymmärrystä string ja character tyyppisistä muuttujista. Seuraavassa koodiesimerkissä näytämme, kuinka voit hakea alimerkkijonon merkkijonosta ja tulostaa sen sarjaliikenteen kautta.

```arduino
String merkkijono = "Tervetuloa Arduino-maailmaan!";
Serial.begin(9600);
Serial.println(merkkijono.substring(11, 19));
```
Tämä koodi tulostaisi "Arduino" sarjaliikenteen näytölle, koska se hakee alimerkkijonon väliltä 11 ja 19 ja tulostaa sen.

Voit myös tallentaa alimerkkijonon uuteen muuttujaan ja käyttää sitä haluamallasi tavalla. Esimerkiksi:

```arduino
String nimi = "Matti Meikäläinen";
String sukunimi = nimi.substring(6, 17);
Serial.begin(9600);
Serial.println(sukunimi);
```

Tämä koodi tulostaisi "Meikäläinen" sarjaliikenteen näytölle, koska se tallentaa alimerkkijonon väliltä 6 ja 17 uuteen muuttujaan ja tulostaa sen.

## Syvempi sukellus

Alimerkkijonojen hakeminen perustuu kahteen tärkeään funktioon: "substring" ja "charAt". "Substring" palauttaa alimerkkijonon valitusta indeksistä toiseen ja "charAt" palauttaa merkin tietystä indeksistä.

On tärkeää huomata, että indeksit alkavat aina nollasta ja että substring-funktiota käytettäessä toinen indeksi on poissuljettu. Esimerkiksi jos haluat hakea alimerkkijonon väliltä 3 ja 8, sinun tulisi käyttää substring(3, 9).

Voit myös käyttää erilaisia ehtolauseita ja looppeja alimerkkijonojen hakemiseen ja käsittelyyn. Esimerkiksi voit käyttää "if" lausetta tarkistaaksesi, sisältääkö merkkijono tietyn alimerkkijonon. Voit myös käyttää for-looppeja käymään läpi merkkijonon merkkejä ja käyttää niitä haluamallasi tavalla.

## Katso myös

- [Arduino-string reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [C++ string documentation](https://www.cplusplus.com/reference/string/string/)
- [String manipulation functions in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)