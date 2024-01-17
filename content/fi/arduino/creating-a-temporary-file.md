---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Arduino: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# What & Why?
Luota minuun, kyllä sinä tarvitset tilapäistiedostoja, vaikka et vielä tiedäkään sitä. Tilapäistiedosto on lyhytaikainen tiedosto, jota ohjelma luo ja käyttää jonkin tietyn tehtävän suorittamiseen. Tämä voi olla esimerkiksi tallennetun tiedon lataaminen, jota ei enää tarvita lopputulokseen.

# How to:
#### Luomme tilapäistiedoston nimen "example.txt" ja kirjoitamme siihen "Tämä on tilapäistiedosto".

Arduino (".txt", "w");  
Arduino.println ("Tämä on tilapäistiedosto");

#### Tulostus näyttää seuraavalta:

Tämä on tilapäistiedosto

# Deep Dive:
Tilapäistiedostojen käyttö on ollut välttämätöntä ohjelmoinnissa jo pitkään. Kyseisillä tiedostoilla voidaan säilyttää tietoa hetkellisesti ohjelman suorituksen aikana. Tämä auttaa välttämään liiallista muistin käyttöä pysyvien tiedostojen käsittelyssä.

On myös olemassa vaihtoehtoja tilapäistiedostoille, kuten dynaamisen muistin käyttö. Kuitenkin tilapäistiedostot ovat yleisesti käytettyjä ja helppokäyttöisiä ratkaisuja.

Tilapäistiedostot luodaan usein ohjelman "tmp" hakemistoon, joka on varattu tällaisten tiedostojen käyttöön. On tärkeää muistaa poistaa tilapäistiedostot käytön jälkeen, jotta ne eivät jätä jälkeä muistiin tai täytä levytilaa.

# See Also:
-https://www.arduino.cc/reference/en/language/functions/filesystem/tmpfile/  
-https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm