---
title:    "Arduino: Merkkijonon pituuden löytäminen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

Miksi: Kaksi tärkeää syytä miksi Arduino-ohjelmoijan olisi hyödyllistä osata löytää merkkijonon pituus, ovat pystyäkseen käsittelemään tekstipohjaista dataa ja tehokkaamman koodaamisen mahdollistaminen.

##Miten tehdä: 

Esimerkki 1: Koodinpätkä, jokavälillä käsetellään kovakoodattua arvoa ja toisessa osassa käytetään String-metodia ```length ()``` merkkijonon pituuden löytämiseksi. Tämä koodi toimii kuin komento, mutta se on paljon joustavampi ja voit lisätä myöhemmin lisäominaisuuksia tarpeesi mukaan.

```
Arduino code
int myString = "This is a string"; 
int stringLength = myString.length(); // Laskee merkkijonon pituuden 
```

Tulostus esimerkki 1:
```
16 
```
Esimerkki 2: Tässä koodissa käytetään for-silmukkaa laskeakseen merkkijonon pituuden, sen sijaan että käytettäisiin String-metodia. Tämä on hyvä tapa ymmärtää, miten String-metodi toimii takana käsin.

```
Arduino code
int myString = "This is a string";
int stringLength = 0;

for(int i = 0; myString[i] != '\0'; i++){ // Jatkaa, kunnes merkkijonon lopussa
  stringLength++; // Lisää yhden pituuteen joka kerta kun kierros loppuu
}
```

Tulostus esimerkki 2:
```
16
```

## Syvemmältä:

Merkkijonon pituuden löytäminen on tärkeä taito Arduino-ohjelmoijille, koska se mahdollistaa paremman hallinnan tekstipohjaisessa datassa ja auttaa tehokkaamman koodaamisen saavuttamisessa. On myös tärkeää huomata, että merkkijonojen pituus voidaan löytää myös C++-kielen avulla, jota Arduino käyttää.

```
C++ code void loop() { 
  String myString = "This is a string"; // Asettaa string "muuttujaan"
  Serial.println(myString.length()); // Tulostaa stringin pituuden 
}
```

Mikäli käytetään C++-kieltä, ei voida käyttää String-metodia ```length()```, koska se on Arduino-kirjaston sisäinen funktio. Tällöin voidaan käyttää algoritmia, joka etsii merkkijonon lopun erikoismerkin ('\0') ja laskee tätä ennen olevien merkkien määrän.

Ymmärtämällä miten merkkijonojen pituuden löytäminen toimii, voi Arduino-ohjelmoija saavuttaa paremman hallinnan tekstimassassa ja pystyy koodaamaan tehokkaammin.

# Katso myös:

- [Arduino String-metodit](https://www.arduino.cc/en/Reference/StringObject)
- [C++ merkkijonojen pituuden laskeminen](https://www.geeksforgeeks.org/program-find-string-length-without-using-length-function/)
- [Arduino C++ oppimateriaali](https://programmingelectronics.com/learn-arduino-c-programming-for-arduino-c/)

Kiitos kun luit tämän blogipostauksen! Toivottavasti se auttoi sinua ymmärtämään merkkijonojen pituuden löytämisen tärkeyden Arduino-ohjelmoinnissa. Onnea ja hauskoja koodailuhetkiä!