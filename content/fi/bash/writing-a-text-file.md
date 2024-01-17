---
title:                "Tekstin kirjoittaminen tiedostoon"
html_title:           "Bash: Tekstin kirjoittaminen tiedostoon"
simple_title:         "Tekstin kirjoittaminen tiedostoon"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Kirjoittaminen teksti-tiedostoon tarkoittaa tiedostoon tallentamista, joka sisältää tekstiä ja mahdollisesti muita tietoja. Ohjelmoijat tekevät sitä tallentaakseen ja käsitelläkseen tietoja, kuten käyttäjän antamia syötteitä, suorituksen tuloksia tai tiedostojen sisältöä.

## Kuinka:

Esimerkkejä koodista ja ne "Bash...`"-lohkoja.

```Bash
# Luodaan ja kirjoitetaan teksti-tiedosto
touch tiedosto.txt # Luodaan tiedosto nimeltä "tiedosto.txt"
echo "Tervetuloa Bashin maailmaan!" > tiedosto.txt # Kirjoitetaan tiedostoon teksti

# Näytetään tiedoston sisältö
cat tiedosto.txt # Tulostetaan tiedoston sisältö

# Lisätään uusi rivi tiedostoon
echo "Olet oppinut uuden taidon!" >> tiedosto.txt # Lisätään uusi rivi tiedostoon

# Näytetään uusi tiedostosisältö
cat tiedosto.txt # Tulostetaan tiedoston uusi sisältö
```

Tuloste:

```
Tervetuloa Bashin maailmaan!
Olet oppinut uuden taidon!
```

## Syvemmälle:

Historiallinen konteksti: Kirjoittaminen teksti-tiedostoon liittyy läheisesti käskyjen suorittamiseen komentotulkin kautta. Aikaisemmissa UNIX-järjestelmissä tiedoston kutsuttiin usein " tekstitiedostoksi" ja sen päätteeksi käytettiin ".txt". Nykypäivänä tiedostonimi voi olla mikä tahansa ja tiedoston sisältö voi olla muu kuin pelkkää tekstiä.

Vaihtoehtoja: Bashin lisäksi muita mahdollisia tapoja kirjoittaa teksti-tiedosto ovat esimerkiksi käyttöliittymäpohjaiset ohjelmat kuten Notepad tai TextEdit, tai toisen ohjelmointikielen, kuten Pythonin, avulla kirjoitettu skripti.

Toteutuksen yksityiskohdat: Bashin "echo" -komento on yksi tapa kirjoittaa tekstiä tiedostoon. Se tulostaa annetun tekstin ja ohjaa tulosteen tiedostoon. Vaihtoehtoinen tapa on käyttää ">>" ja neliö aaltosulkeita (">>[tekstitiedosto]") kirjoittaaksesi tiedostoon lisää sisältöä ilman, että uusi sisältö korvaa vanhan sisällön.

## Katso myös:

- "Bash Scripting Tutorial" - Bashin perusteet ja käytännön esimerkkejä: https://ryanstutorials.net/bash-scripting-tutorial/
- "Redirecting Output to a File" - Lisätietoa tiedoston kirjoittamisesta Bashissa: https://www.tldp.org/LDP/abs/html/io-redirection.html#APPENDFILE
- "Creating a Text File in Bash" - Asiantuntijaoppaan ohjeita tiedoston luomiseen Bashissa: https://www.howtogeek.com/657590/creating-a-text-file-in-bash-scripts/