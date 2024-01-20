---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Ojennus termi tarkoittaa merkkijonoon arvoksi muuttamista, usein muuttujia, suoraan. Ohjelmoijat käyttävät sitä koodin mukautettavuuden ja luettavuuden parantamiseksi varmistaen, että koodi on tehokas ja helppo lukea.

## Miten:

```Arduino
String nimi = "Pekka";
String tervehdys = "Hei, " + nimi + "!";
Serial.begin(9600);
Serial.println(tervehdys);
```

*Näytteen ulostulo*: 

```
Hei, Pekka!
```
## Syvä sukellus:

Historiallinen tieto: Merkkijonojen ojennus on ollut kehitysympäristöissä jo pitkään. Sen käyttö on levinnyt yleisesti, koska se tekee koodista helpommin ymmärrettävän ja luettavamman.

Vaihtoehdot: Arduino ei tarjoa kovinkaan monia sisäänrakennettuja vaihtoehtoja merkkijonojen ojennukseen kuten modernit ohjelmointikielet tekevät. Joissakin tapauksissa merkkijonojen yhdistäminen '+'-operaattorilla voi toimia.

Toteutus: Merkkijonojen ojennus edellyttää, että muuttujat ovat String-tyyppejä. Tämä mahdollistaa muuttujan arvon liittämisen suoraan lähdekoodin merkkijonoon.

## Katso myös:

[Arduino String Ojennus](http://www.arduino.cc/en/Tutorial/StringAppendOperator) - Lisätietoa merkkijonojen ojennuksesta Arduinossa.
[String Concatenation](https://www.arduino.cc/en/Tutorial/Strings) - Laajempi katsaus merkkijonoihin Arduinossa, mukaan lukien ojennus.
[Kokoelma käyttökelpoisia merkkijonofunktioita](http://playground.arduino.cc/Main/StringFunctions) - Toimintojen kokoelma merkkijonokäsittelyn tehokkuuden parantamiseksi-