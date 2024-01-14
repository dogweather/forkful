---
title:    "Python: Tekstitiedoston lukeminen"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi?

Tekstien lukeminen on yksi tärkeimmistä taidoista, joita ohjelmoijan tulee hallita. Monissa ohjelmissa tarvitaan mahdollisuus lukea ja käsitellä tekstitiedostoja, joten on tärkeää ymmärtää, miten tämä tapahtuu Python-ohjelmoinnissa. Tässä blogikirjoituksessa käymme läpi perusasiat tekstitiedostojen lukemisesta ja näytämme esimerkkien avulla, kuinka voit tehdä sen omassa ohjelmassasi.

## Miten?

Tekstitiedostojen lukeminen on helppoa Pythonissa. Voit käyttää open() -funktiota, joka avaa tiedoston annettua polkua käyttäen. Tässä on esimerkki, joka luo uuden tiedoston, kirjoittaa siihen tekstin ja lopuksi lukee sen sisällön:

```python
# Avataan tiedosto teksti-tilassa
file = open("teksti.txt", "w+")

# Kirjoitetaan tiedostoon tekstiä
file.write("Tervetuloa lukemaan tekstiä!")

# Nollataan kursori ja luetaan tiedoston sisältö
file.seek(0)
print(file.read())

# Suljetaan tiedosto
file.close()

# Output:
# Tervetuloa lukemaan tekstiä!
```

## Syvempää sukellusta

Tekstitiedostojen lukeminen vaihtelee hieman riippuen siitä, millainen tiedosto on kyseessä. Jos esimerkiksi tiedostossa on eri riveillä olevia sanoja, voit käyttää split() -funktiota saadaksesi sanat talteen listana. Tässä on esimerkki, jossa luetiin tiedoston sisältö rivi kerrallaan ja palautetaan jokainen rivi listana:

```python
# Avataan tiedosto teksti-tilassa
file = open("tehtävät.txt")

# Luetaan rivi kerrallaan
for line in file:
    # Splitataan rivi ja tallennetaan sanat listaan
    words = line.split()
    print(words)

# Output:
# ['Ohjelmointi', 'on', 'hauskaa', '!']
# ['Pythonilla', 'koodaaminen', 'on', 'helppoa', '.']
```

Kuten näet, tekstien lukeminen Pythonissa on hyvin suoraviivaista ja monipuolista. Voit myös käyttää erilaisia tekstin käsittelyyn tarkoitettuja funktioita, kuten replace() ja strip(), joka poistaa ylimääräiset välilyönnit ja rivinvaihdot. Toivottavasti tämä blogikirjoitus auttoi sinua ymmärtämään tekstitiedostojen lukemisen perusteet Pythonissa!

## Katso myös

- [Pythonin virallinen dokumentaatio tekstiä merkkijonona käsittelemisestä](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [Python-kurssin tekstiä käsittelevä luento](https://www.youtube.com/watch?v=QiMdS7sEngA)
- [Pythonin dokumentaatio tekstitiedostojen lukemisesta ja kirjoittamisesta](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)