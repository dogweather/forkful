---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Tiedoston lukeminen tarkoittaa tietojen noutamista tekstidokumentista. Ohjelmoijat lukevat tiedostoja silloin, kun heidän on haettava ja käsiteltävä tallennettua tietoa, kuten tekstitiedostoja.

## Kuinka:

Alla on esimerkkejä koodista, jossa luetaan tekstitiedosto Pythonilla.

```Python
# Avaa ja lue tiedosto
with open('esimerkki.txt', 'r') as tiedosto:
    sisalto = tiedosto.read()
print(sisalto)
```

Tämä koodi avaa tiedoston nimeltä "esimerkki.txt" lukutilassa ('r') ja tulostaa sen sisällön.

## Syvempi sukellus:

Historiallinen konteksti: Tiedostojen lukeminen on ollut ohjelmoinnin perusominaisuus alkuaikojen tilan varaamisesta muistissa tekstiedostoille.

Vaihtoehdot: Voit lukea tiedoston sisällön myös riveittäin `readlines` funktion avulla. Kyseessä on hyödyllinen työkalu, kun tiedosto on suuri tai tiedostosta pitää lukea vain tiettyjä rivejä.

Vastaavat yksityiskohdat: Pythonissa `open` on sisäänrakennettu funktio, jota käytetään tiedostojen avaamiseen. Se palauttaa "_file object_", jota voidaan käyttää tiedoston lukemiseen tai kirjoittamiseen.

## Katso myös:

Lisätietoja Pythonin kanssa tiedostojen käsittelystä:

1. Pythonin virallinen dokumentaatio: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
2. W3Schools Python - Tiedostojen lukeminen: https://www.w3schools.com/python/python_file_open.asp
3. Real Python - Tiedostonkäsittely Pythonissa: https://realpython.com/read-write-files-python/