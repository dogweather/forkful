---
title:                "Arduino: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Miksi poistaa merkkejä, jotka vastaavat kaavaa?

Arduino on suosittu mikrokontrollerijärjestelmä, jota käytetään laajasti tekemään erilaisia elektronisia projekteja. Yksi tärkeimmistä tehtävistä Arduino-ohjelmointissa on käsitellä merkkijonoja, eli tekstidataa. Joskus saattaa olla tarpeen poistaa tiettyjä merkkejä merkkijonosta, jos ne esimerkiksi haittaavat tietojen käsittelyä tai tulostusta.

# Kuinka poistaa merkkejä Arduino-ohjelmoinnissa?

On olemassa monia tapoja poistaa merkkejä Arduino-ohjelmassa, mutta yksi yksinkertainen tapa on käyttää ```replace()``` -funktiota. Tämä funktio korvaa määritetyn merkkijonon uudella merkkijonolla. Alla on esimerkki, jossa ```replace()``` -funktiota käytetään poistamaan kaikki tyhjät välilyönnit merkkijonosta:

```
Arduino String example = "Hello   World";
example.replace(" ", "");
Serial.println(example); // Output: HelloWorld
```

# Syvemmälle merkkien poistamiseen

Merkkien poistaminen voi olla hyödyllistä myös silloin, kun halutaan poistaa tietyt merkit, jotka vastaavat tiettyä kaavaa. Tämä voidaan tehdä esimerkiksi käyttäen ```if``` -lauseita ja ```substring()``` -funktiota, joka palauttaa halutun osan merkkijonosta.

```
Arduino String example = "1234abc";
for (int i = 0; i < example.length(); i++) {
  if (isDigit(example.charAt(i))) {
    String number = example.substring(i);
    Serial.println(number); // Output: 1234
    break;
  }
}
```

Tässä esimerkissä merkkiä, joka vastaa kaavaa ```[0-9]```, vastaava osa merkkijonosta on tallennettu uuteen String-muuttujaan ja tulostettu sarjaporttiin. Muutaman lisämuutoksen avulla voit helposti mukauttaa tätä koodia poistamaan muita kaavoja vastaavia merkkejä.

# Katso myös

- [Arduino String-referenssi](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [String-tiedoston korvaaminen substring-jono](https://www.instructables.com/Replacing-a-Substring-in-a-String-with-an-Arduino/)