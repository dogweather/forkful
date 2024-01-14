---
title:                "Python: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedostojen kirjoittaminen on tärkeä osa Python-ohjelmoinnin oppimista. Se on tapa tallentaa ja järjestää tietoa pysyvästi sekä jakaa sitä muiden kanssa.

## Miten

Käytännön esimerkki: Tallennetaan lista nimistä tekstitiedostoon nimeltä "henkilöt.txt".

```Python
# Luodaan lista henkilöiden nimistä
henkilot = ["Maria", "Juhani", "Anna", "Mikko"]

# Avataan tiedosto kirjoitusmoodissa, lisätään "w" sulkeiden loppuun
tiedosto = open("henkilöt.txt", "w")

# Kirjoitetaan jokainen nimi omalle riville tiedostoon
for nimi in henkilot:
    tiedosto.write(nimi + "\n")

# Suljetaan tiedosto
tiedosto.close()

# Tulostetaan viesti onnistuneesta tallennuksesta
print("Tiedosto 'henkilöt.txt' tallennettu!")

# Lopputulos:
# Tiedosto 'henkilöt.txt' tallennettu!
```

## Syvällisemmin

Tekstitiedostoja kirjoitettaessa on tärkeää muistaa muutamia asioita. Ensinnäkin, tiedoston avaaminen "w" tilassa tarkoittaa sitä, että kaikki olemassa olevat tiedot tiedostossa poistetaan ja uudet tiedot kirjoitetaan sen tilalle. Tiedoston avattuasi muista sulkea se `close()` komennolla.

Voit myös avata tiedoston lukutilassa "r" ja lisätä uutta tietoa tiedoston loppuun `append()` komennolla. Samoin voit lukea tiedoston sisällön `read()` komennolla.

## Katso myös

- [Pythonin tekstikäsittely](https://www.python.org/dev/peps/pep-0008/#code-lay-out)
- [Markdown-kieli](https://daringfireball.net/projects/markdown/)