---
title:                "Bash: Tiedoston kirjoittaminen"
simple_title:         "Tiedoston kirjoittaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Bash-ohjelmointi on tärkeä taito, joka voi auttaa helpottamaan monia arkipäivän tehtäviä. Yksi näistä tehtävistä on tekstitiedoston kirjoittaminen. Tässä blogikirjoituksessa käymme läpi, miksi tekstitiedoston kirjoittaminen kannattaa ja miten se onnistuu Bash-ohjelmoinnilla.

## Miten

Bash-ohjelmoinnissa tekstitiedoston kirjoittaminen on helppoa ja nopeaa. Voit luoda uuden tiedoston tai päivittää olemassa olevaa tiedostoa käyttämällä `touch` tai `echo` komentoja. Esimerkiksi:

```
# Luo uusi tiedosto nimeltä testi.txt
touch testi.txt

# Lisää teksti tiedostoon käyttämällä echo-komentoa
echo "Tämä on testiä" >> testi.txt
```
Tämän jälkeen voit tarkistaa tiedoston sisällön käyttämällä `cat` komentoa:

```
# Tulosta tiedoston sisältö konsoliin
cat testi.txt

Tämä on testiä
```

Käytä `>`-merkkiä, jos haluat ylikirjoittaa olemassa olevan tiedoston sisällön:

```
# Korvaa tiedoston sisältö uudella tekstillä
echo "Uusi sisältö" > testi.txt
```

## Syvemmälle

Bash-ohjelmoinnissa on mahdollista myös lisätä muuttujia ja käyttää for-silmukoita tekstitiedoston kirjoittamisessa. Esimerkiksi voit kirjoittaa tiedostoon oven numerot käyttämällä for-silmukkaa:

```
# Luo uusi tiedosto nimeltä ovet.txt
touch ovet.txt

# Luo muuttuja, jossa on ovi numerot 1-5
ovet="1 2 3 4 5"

# Käytä for-silmukkaa tulostamaan muuttujan arvot ja lisää ne tiedostoon
for ovi in $ovet
do
  echo "Ovi $ovi" >> ovet.txt
done
```

Tiedoston sisältö näyttää nyt tältä:

```
Ovi 1
Ovi 2
Ovi 3
Ovi 4
Ovi 5
```

## Katso myös

- [Bash Scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial)
- [Muokkaa tekstiä Bash-ohjelmoinnilla](https://www.computerhope.com/unix/bash/echo.htm)
- [Bash ohjelmointiopas](https://www.gnu.org/software/bash/manual/html_node/Bash-Programmable-Completion.html)