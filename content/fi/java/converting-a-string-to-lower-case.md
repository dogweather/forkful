---
title:    "Java: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisi muuntaa merkkijono pienaikaiseksi? Tämä on yleinen tarve monissa Java-ohjelmointiprojekteissa, joissa käyttäjien syötteet on tarkistettava ja vertailtava. Pienaikaiset merkkijonot ovat myös helpompia käsitellä ja vertailla, mikä tekee koodista selkeämpää ja tehokkaampaa.

## Miten

Pienaikaisen merkkijonon luominen Java-ohjelmassa on helppoa käyttämällä String-luokan toLowercase() -metodia. Alla olevassa koodiesimerkissä näytetään, miten muuttaa syötetty merkkijono pienaikaiseksi ja tulostaa se konsoliin:

```Java
// Syötetty merkkijono
String s = "TÄMÄ ON TÄRKEÄÄ";
// Kutsutaan toLowercase() -metodia
String lowerCaseString = s.toLowerCase();
// Tulostetaan pienaikainen merkkijono
System.out.println(lowerCaseString);
```

Koodin tulostama tulos on "tämä on tärkeää". Huomaa, että koodi on helposti muokattavissa ja voi käsitellä erilaisia merkkijonoja.

## Syväsukeltaminen

Pienaikaisen merkkijonon taustalla on Unicode-standardi, joka määrittelee jokaiselle merkille uniikin numerokoodin. Suuri ja pieni kirjain eroavat toisistaan numerokoodinsa perusteella. ToLowercase() -metodi lukee merkkijonon jokaisen merkin numerokoodin ja vähentää siitä tarvittaessa tietyn määrän. Tällä tavalla se pystyy muuttamaan suuret kirjaimet pieniksi kirjaimiksi.

On myös hyvä huomioida, että toLowercase() -metodi käyttää laitteiston paikallista asetusta määrittääkseen, mitkä merkit ovat suuria ja pieniä kirjaimia. Tämä voi johtaa eroihin eri kielten välillä.

## Katso myös

- [Java String-luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Unicode-standardi](https://unicode.org/charts/)
- [Java merkkijonotutoriaali (englanniksi)](https://www.javatpoint.com/java-string-to-lowercase)