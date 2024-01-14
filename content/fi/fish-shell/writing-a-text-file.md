---
title:                "Fish Shell: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedoston kirjoittaminen voi olla hyödyllistä esimerkiksi, kun haluat tallentaa tietoa pysyvästi tai luoda skriptin toistuvien tehtävien suorittamiseen.

## Kuinka tehdä

Fish Shell tarjoaa helpon tavan kirjoittaa tekstitiedostoja. Voit käyttää 'echo' -komentoa yhdessä '>>' -merkin kanssa tallentaaksesi tiedoston sisällön tekstiksi. On myös tärkeää huomata, että voit käyttää '>>' -symbolia luodaksesi uuden tiedoston, ja '>>>' symbolia lisätäksesi sisältöä jo olemassa olevaan tiedostoon.

```Fish Shell
# Luodaan uusi tiedosto nimeltä 'esimerkki.txt'
echo "Tämä on esimerkki tekstistä" >> esimerkki.txt

# Lisätään uusi rivi olemassa olevaan tiedostoon 'esimerkki.txt'
echo "Tämä on uusi rivi" >>> esimerkki.txt
```

Tämän komennon suorittamisen jälkeen voit nähdä, että tiedoston 'esimerkki.txt' sisältö on päivittynyt vastaavasti.

## Syvennä

Tekstitiedoston kirjoittaminen voi vaikuttaa yksinkertaiselta, mutta on tärkeää ymmärtää, miten tiedostojen kirjoittaminen toimii käyttöjärjestelmän näkökulmasta. Kun käytät 'echo' komentoa >>-merkillä, Fish Shell lisää uuden rivin tiedoston pääteleen. Tämä tarkoittaa, että jos et anna uutta riviä komennon jälkeen, se saattaa lisätä uuden rivin ja tehdä tiedoston lukemisen vaikeammaksi.

## Katso myös

- [Fish Shell dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Opas tekstiedistämiseen Fish Shellilla](https://www.digitalocean.com/community/tutorials/how-to-do-string-manipulation-in-fish-shell-scripts)
- [Fish Shellin erikoismerkkien opas](https://fishshell.com/docs/current/tutorial.html#tutorial-special-vars)