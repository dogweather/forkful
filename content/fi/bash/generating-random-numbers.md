---
title:                "Satunnaislukujen generointi"
html_title:           "Bash: Satunnaislukujen generointi"
simple_title:         "Satunnaislukujen generointi"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Satunnaislukujen generointi on tärkeä osa ohjelmointia, sillä se antaa mahdollisuuden luoda tietokoneohjelmia, jotka toimivat ennustamattomalla tavalla. Satunnaisuutta tarvitaan esimerkiksi peleissä, salasanoiden luomisessa ja tietojen salaamisessa.

## Miten:
Alla on esimerkkejä siitä, miten satunnaislukuja voidaan generoida Bashilla:

```Bash
# Generoi satunnainen kokonaisluku väliltä 1-10
echo $(( $RANDOM % 10 + 1 ))

# Generoi satunnainen desimaaliluku väliltä 0-1
echo "scale=2; $RANDOM/32767" | bc
```

Esimerkkituloste:

```Bash
6
0.25
```

## Syvennys:
Satunnaislukujen generointi on ollut osa tietokoneiden kehitystä jo vuosikymmenien ajan. Ennen digitaalisten tietokoneiden aikakautta, satunnaislukuja pyrittiin luomaan mekaanisilla laitteilla kuten noppilla ja ruleteilla. Nykypäivänä satunnaisuutta pystytään luomaan tarkemmin ja nopeammin tietokoneilla.

On olemassa myös muita tapoja generoida satunnaislukuja kuin Bashin sisäisen $RANDOM-muuttujan avulla. Esimerkiksi Pythonin random-moduuli tarjoaa laajempia toimintoja satunnaisuuden hallintaan.

Bashin satunnaislukufunktio perustuu algoritmiin, joka käyttää ajasta ja käyttäjän ID:stä lasketun siemenluvun perusteella pseudo-satunnaislukugeneraattoria. Tämä tarkoittaa, että samaan aikaan suoritetun ohjelman ajoista tulee samat tulokset. 

## Katso myös:
- [Bash-komentoriviohjeet](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [Random-moduulin dokumentaatio Pythonissa](https://docs.python.org/3/library/random.html)