---
title:    "Arduino: Merkkijonon muuttaminen isoin kirjaimin"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi?

Miksi haluaisit muuttaa kirjainkokoa merkkijonossa? Saattaa kuulostaa turhalta, mutta joskus on hyödyllistä muuttaa merkkijonon kirjaimia isoihin tai pieniin kirjaimiin. Tämä voi auttaa helpottamaan tietojen syöttämistä ja verrattavuutta.

## Kuinka tehdä?

Arduino-koodilla on helppo muuttaa merkkijonon kirjainkokoja. Käytämme siihen string-olion capitalize() -funktiota. Se toimii seuraavasti:

```Arduino
string merkkijono = "Hei kaikki!";
String isoiksi = merkkijono.capitalize();
```

Tuloksena saat "Hei kaikki!" -merkkijonon, jossa ensimmäinen kirjain on muutettu isoksi kirjaimeksi, muut pieniksi.

Voit myös muuttaa merkkijonossa olevia tiettyjä kirjaimia käyttämällä String-olion replace() -funktiota. Esimerkiksi, jos haluat muuttaa kaikki "i" -kirjaimet "e":ksi, käytä seuraavaa koodia:

```Arduino
string merkkijono = "Pii2";
String uusiMerkkijono = merkkijono.replace("i", "e");
```

Tuloksena saat "Pee2" -merkkijonon. Voit käyttää myös muita string-olioon kuuluvia funktioita, kuten toLowerCase() ja toUpperCase(), muuttaaksesi merkkijonon kirjainkokoja.

## Syvempää tietoa

Merkkijonon kirjainkoon muuttamisella on muutakin käyttöä kuin vain tietojen helpottaminen. Esimerkiksi, joissakin tapauksissa vertaaminen merkkijonossa olevia sanoja voi olla hankalaa, jos ne ovat eri kirjainkoossa. Muuttamalla ne kaikki samaksi, tietojen vertailu ja käsittely helpottuu.

On myös tärkeää huomata, että merkkijonon capitalize() -funktio muuttaa vain merkkijonon ensimmäisen kirjaimen, muut kirjaimet säilyvät samoina. Jos haluat muuttaa kaikki kirjaimet isoihin tai pieniin kirjaimiin, voit käyttää toUpperCase() tai toLowerCase() -funktioita.

## Katso myös

- [String-olion dokumentaatio](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [mikrokontrollerin merkkijonon muokkaaminen](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)