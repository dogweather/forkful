---
title:                "Stringin muuttaminen pieniksi merkeiksi."
html_title:           "Arduino: Stringin muuttaminen pieniksi merkeiksi."
simple_title:         "Stringin muuttaminen pieniksi merkeiksi."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Mikä on merkkijonon muuttaminen pieniksi kirjaimiksi ja miksi ohjelmoijat tekevät sitä? Tämä on prosessi, jossa merkkijonon kaikki kirjaimet muutetaan pieniksi kirjaimiksi. Tämä on tärkeää, koska ohjelmointikielet ovat herkkiä kirjainten koosta, joten tekstin yhdenmukaisuuden varmistamiseksi on parempi käyttää vain pieniä kirjaimia.

# Kuinka tehdä:

```
ArduinoString teksti = "TÄMÄ ON ESIMERKKI";

for (int i = 0; i < teksti.length(); i++) {
  teksti[i] = tolower(teksti[i]);
}

Serial.println(teksti); 
```

Tässä esimerkkikoodissa luomme merkkijonon nimeltä "teksti" ja tallennamme siihen arvon "TÄMÄ ON ESIMERKKI". Sitten käytämme for-silmukkaa, joka käy läpi koko merkkijonon ja muuttaa jokaisen kirjaimen pieneksi käyttämällä "tolower" -funktiota. Lopuksi tulostamme muutetun merkkijonon sarjanäyttöön, mikä näyttää seuraavalta: "tämä on esimerkki".

# Syvällinen sukellus:

Merkkijonon muuntaminen pieniksi kirjaimiksi on tärkeää tietokoneohjelmoinnissa, koska se auttaa välttämään virheitä, jotka johtuvat erilaisten kirjainten käytöstä. Tämän funktion historia juontaa juurensa ASCII-koodaukseen, jossa kirjaimet ja symbolit ovat edustettuina numeroina. On myös muita tapoja toteuttaa tämä toiminto, kuten käyttämällä erilaisia kirjastoja ja funktioita, jotka ovat saatavilla eri ohjelmointikielillä.

# Katso myös:

Tässä on lisäresursseja, jotka voivat auttaa sinua ymmärtämään merkkijonon muuntamista pieniksi kirjaimiksi:

- https://www.arduino.cc/reference/en/language/variables/data-types/string/ - lisätietoja "String" -luokasta ja sen toiminnoista Arduino-kielellä.
- https://www.geeksforgeeks.org/to_lower-function-in-c/ - selvempi kuvaus "tolower" -funktiosta ja sen käytöstä.
- https://www.rapidtables.com/code/text/ascii-table.html - ASCII-kooditaulukko.