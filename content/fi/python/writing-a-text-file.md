---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Python: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi tekstitiedoston kirjoittaminen voisi olla hyödyllistä. Esimerkiksi voit tallentaa tietoja pysyvästi tai hallita suuria tietomääriä helposti.

## Miten

Annamme esimerkkejä siitä, miten voit kirjoittaa Pythonilla tekstitiedoston, ja tulostaa sen sisällön:

```Python
# Avaa tiedosto nimeltä "esimerkki.txt" ja kirjoita siihen tekstiä
file = open("esimerkki.txt", "w")
file.write("Tämä on Pythonin esimerkkitiedosto")
file.close() # Sulje tiedosto

# Avaa tiedosto uudelleen ja lue sen sisältö
file = open("esimerkki.txt", "r")
print(file.read()) # Tulosta tiedoston sisältö
file.close() # Sulje tiedosto

# Output:
# Tämä on Pythonin esimerkkitiedosto
```

## Syventyminen

Voit myös lisätä uusia rivejä ja muokata tiedoston sisältöä. Tässä esimerkissä lisätään tiedostoon uusi rivi ja korvataan ensimmäinen rivi uudella tekstillä:

```Python
file = open("esimerkki.txt", "a") # Avaa tiedosto lisäystilassa
file.write("\nTämä on uusi rivi") # Lisää uusi rivi loppuun
file.close() # Sulje tiedosto

# Avaa tiedosto ja lue sen sisältö uudelleen
file = open("esimerkki.txt", "r")
print(file.read()) # Tulosta tiedoston sisältö
file.close() # Sulje tiedosto

# Output:
# Tämä on Pythonin esimerkkitiedosto
# Tämä on uusi rivi
```

## Katso myös

- [Pythonin virallinen dokumentaatio tekstitiedoston kirjoittamisesta](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Lyhyt opetusvideo tekstitiedoston kirjoittamisesta Pythonilla](https://www.youtube.com/watch?v=hDSVInZ2ARE)