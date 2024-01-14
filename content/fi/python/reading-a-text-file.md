---
title:    "Python: Tekstitiedoston lukeminen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi?

Lukeminen ja kirjoittaminen ovat olennainen osa ohjelmointia. Tekstitiedostojen lukeminen on tärkeä taito, jota kaikki ohjelmoijat tarvitsevat käyttäessään Pythonia. Tekstitiedostoissa on usein tallennettuna tärkeitä tietoja, kuten käyttäjätietoja, tiedostojen polkuja tai konfiguraatioasetuksia. Näiden tietojen saaminen ja käsitteleminen voi joskus olla haastavaa, mutta onneksi Pythonilla se on helppoa ja tehokasta.

## Miten tehdä?

Voit avata ja lukea tekstitiedostoja helposti käyttämällä `open()`-funktiota. Tämä funktio hyödyntää `with`-lausetta, mikä varmistaa, että tiedosto suljetaan automaattisesti lukemisen jälkeen. Tämän jälkeen voit käyttää `read()`-metodia lukeaksesi tiedoston sisällön ja tallentaa sen muuttujalle. Alla on esimerkki, jossa avataan tekstitiedosto nimeltä "tekstitiedosto.txt" ja luetaan sen sisältö tulostamalla se konsoliin:

```Python
with open("tekstitiedosto.txt", "r") as tiedosto:
    sisalto = tiedosto.read()
    print(sisalto)
```

Tämä tulostaa tekstitiedoston sisällön konsoliin seuraavassa muodossa:

```
Tämä on esimerkki tekstitiedostosta.
Voit tallentaa tähän tärkeitä tietoja.
```

Voit myös käyttää muita lukemismetodeja, kuten `readline()`, joka lukee yhden rivin kerrallaan, ja `readlines()`, joka lukee kaikki rivit ja tallentaa ne listana.

## Syvemmälle aiheeseen

Pythonilla voit myös käsitellä tekstitiedostojen rivi kerrallaan `for`-loopin avulla. Alla on esimerkki, jossa käydään läpi tiedoston rivit ja tulostetaan kukin rivi konsoliin:

```Python
with open("tekstitiedosto.txt", "r") as tiedosto:
    for rivi in tiedosto:
        print(rivi)
```

Voit myös kirjoittaa ja tallentaa tekstitiedostoja `write()`-metodin avulla. Alla on esimerkki, jossa luodaan uusi tekstitiedosto nimeltä "uusi_tiedosto.txt" ja kirjoitetaan siihen tietoja:

```Python
with open("uusi_tiedosto.txt", "w") as tiedosto:
    tiedosto.write("Tämä on uusi tiedosto.")
    tiedosto.write("Sisältö tallennetaan tähän.")
```

## Katso myös

- [Pythonin `open()`-funktio](https://docs.python.org/3/library/functions.html#open)
- [Pythonin tiedostotoiminnot](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Markdownin käyttöönotto Pythonissa](https://www.python.org/dev/peps/pep-0008/#id50)