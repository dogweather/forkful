---
title:                "Python: Tekstitiedoston lukeminen"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedostojen lukeminen on tärkeä taito jokaiselle Python-ohjelmoijalle. Se antaa sinulle mahdollisuuden käsitellä suurta määrää tietoa ja tehdä muutoksia tiedostoihin nopeasti ja helposti.

## Kuinka

Tekstitiedoston lukeminen Python-ohjelmassa on hyvin yksinkertaista. Sinun täytyy vain avata tiedosto, lukea sen sisältö ja sulkea se. Tässä on esimerkki koodista ja tulosteesta:

```Python
# Avaa tiedosto lukemista varten
tiedosto = open("tiedosto.txt", "r")
# Lue ja tulosta tiedoston sisältö
sisalto = tiedosto.read()
print(sisalto)
# Sulje tiedosto
tiedosto.close()
```

Tämä koodi avaa tiedoston nimeltä "tiedosto.txt" ja lukee sen sisällön muuttujaan nimeltä "sisalto". Sitten se tulostaa sisällön konsoliin ja lopuksi sulkee tiedoston. 

## Syvemmälle

Voit myös lukea tiedostoa rivi kerrallaan käyttämällä "readlines()" -metodia:

```Python
# Avaa tiedosto lukemista varten
tiedosto = open("tiedosto.txt", "r")
# Lue tiedoston sisältö ja tallenna rivit listaan
rivit = tiedosto.readlines()
# Tulosta jokainen rivi erikseen
for rivi in rivit:
  print(rivi)
# Sulje tiedosto
tiedosto.close()
```

Voit myös avata, lukea ja sulkea tiedoston yhdellä komennolla "with", joka huolehtii automaattisesti tiedoston sulkemisesta:

```Python
# Avaa tiedosto lukemista varten
with open("tiedosto.txt", "r") as tiedosto:
  # Käsittele tiedoston sisältö tässä
  # ...
# Tiedosto suljetaan automaattisesti tämän jälkeen
```

## Katso myös

- [Pythonin dokumentaatio tekstitiedoston lukemisesta](https://docs.python.org/fi/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorialspointin opas tekstitiedoston lukemiseen Pythonilla](https://www.tutorialspoint.com/python/python_files_io.htm)
- [GeeksforGeeksin esimerkkejä tekstitiedoston lukemisesta Pythonilla](https://www.geeksforgeeks.org/reading-writing-text-files-python/)