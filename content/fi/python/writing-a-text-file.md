---
title:    "Python: Tekstitiedoston kirjoittaminen"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi tekstitiedoston kirjoittaminen voi olla hyödyllistä Python-ohjelmoijalle. Se voi auttaa tallentamaan ja jakamaan tietoa, luomaan tietokantoja tai jopa luomaan sisältöä verkkosivuille.

## Kuinka

Tekstitiedoston kirjoittaminen Pythonilla on helppoa. Se voi tapahtua käyttämällä sisäänrakennettua `open()` -funktiota ja `write()` -metodia.

```Python
# Avaa tiedosto tiedostoon "teksti.txt" ja valitse kirjoittamisasetukset
with open("teksti.txt", "w") as tiedosto:
    #Kirjoita haluamasi teksti tiedostoon
    tiedosto.write("Tässä on tekstiä, jota voidaan tallentaa tiedostoon.") 
```

Kun suoritat tämän koodin, se luo tekstitiedoston nimeltä "teksti.txt" ja tallentaa annetun tekstin siihen. Voit myös käyttää `"a"`-muotoa, joka avaa tiedoston lisäämistä varten, jos haluat lisätä tekstiä jo olemassa olevaan tiedostoon.

## Syvällinen sukellus

Pythonilla on monia erilaisia tapoja kirjoittaa ja muokata tekstitiedostoja. Voit esimerkiksi lisätä rivinvaihtoja tai luoda luetteloita koodin avulla. Lisäksi voit käyttää `read()`-metodia lukemaan tiedostosta sisältöä takaisin ohjelmaan.

```Python
# Avaa tiedosto ja lue sen sisältö muuttujaan "sisalto"
with open("teksti.txt", "r") as tiedosto:
    sisalto = tiedosto.read() 
# Tulostaa muuttujan "sisalto" sisällön
print(sisalto)
```

Voit myös asettaa `encoding`-parametrin `open()`-funktiolle, jotta voidaan käsitellä erityisiä merkistöjä ja välttää mahdollisia koodausongelmia.

## Katso myös

- [Pythonin virallinen dokumentaatio tiedostojen käsittelyyn](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tekstiedoston kirjoittaminen ja lukeminen Pythonilla - GeeksforGeeks (englanniksi)](https://www.geeksforgeeks.org/reading-writing-text-files-python/)
- [Kehittyneempi opas tiedostojen käsittelyyn Python-kansiosta (englanniksi)]https://realpython.com/read-write-files-python/