---
title:    "Python: Tiedoston kirjoittaminen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittamalla tekstitiedostoja voit tallentaa pysyvää tietoa ohjelmistosi suorituksista, suodattaa ja analysoida sitä myöhemmin tai jakaa sitä muiden kanssa. Tämä on hyödyllistä, kun haluat tallentaa tietoa suurista tietomassoista tai luoda raportin suorituksista.

## Kuinka kirjoittaa tekstitiedosto

```python
# Kirjoitetaan tekstitiedosto nimeltä "esimerkki.txt"
tiedosto = open("esimerkki.txt", "w")
# Kirjoitetaan tiedostoon rivi tekstiä
tiedosto.write("Tämä on esimerkki tekstiriviltä.")
# Lisätään toinen rivi tiedostoon
tiedosto.write("Tässä on toinen rivi tekstiä.")
# Suljetaan tiedosto
tiedosto.close()
```

```python
# Luetaan juuri luotu tiedosto
tiedosto = open("esimerkki.txt", "r")
# Tallennetaan tiedoston sisältö muuttujaan
sisältö = tiedosto.read()
# Tulostetaan sisältö
print(sisältö)
```

### Tulos:

Tämä on esimerkki tekstiriviltä.
Tässä on toinen rivi tekstiä.

## Syvempää tietoa tekstitiedoston kirjoittamisesta

Tekstitiedoston kirjoittaminen muodostaa usein käyttäjän ja datan välisen ensimmäisen vuorovaikutuksen. Pythonissa on useita erilaisia tapoja kirjoittaa tekstiä tiedostoon, mutta yleisin tapa on käyttää `open()`-funktiota, jossa ensimmäinen parametri on tiedoston nimi ja toinen parametri määrittelee avausmuodon, joka tässä tapauksessa on "w" eli kirjoitusmuoto.

On tärkeää muistaa sulkea tiedosto `close()`-funktiolla, kun tiedoston kirjoittaminen on valmis. Vasta sulkemisen jälkeen lähetetään tiedosto oikeasti tietokoneelle tallennettavaksi.

Voit myös käyttää `with`-lauseketta, jossa tiedoston käsittely sulkeutumisineen hoidetaan automaattisesti.

```python
with open("esimerkki.txt", "w") as tiedosto:
    # Kirjoitetaan tiedostoon
    tiedosto.write("Tämä on esimerkki tekstiriviltä.")

# Tiedosto suljetaan automaattisesti.
```

## Katso myös

- [Pythonin virallinen dokumentaatio](https://docs.python.org/fi/3/library/functions.html?highlight=open#open)
- [RealPython: Python Files](https://realpython.com/read-write-files-python/#writing-files)
- [Python Tutorial: Write to a file](https://www.programiz.com/python-programming/file-operation#writing)