---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Tekstitiedoston kirjoittaminen tarkoittaa informaation tallentamista pysyvään muotoon tiedostoon. Ohjelmoijat kirjoittavat tiedostoja dataa säilyttämään, asetuksia tallentaakseen ja kommunikoimaan eri prosessien tai järjestelmien välillä.

## How to: - Miten:
```Python
# Tekstin kirjoittaminen tiedostoon
with open('tervehdys.txt', 'w') as tiedosto:
    tiedosto.write('Hei suomalaiset ohjelmoijat!\n')

# Tiedoston lukeminen
with open('tervehdys.txt', 'r') as tiedosto:
    sisalto = tiedosto.read()
    print(sisalto)
```
Output:
```
Hei suomalaiset ohjelmoijat!
```
```Python
# Useamman rivin kirjoittaminen tiedostoon
rivit = ['Rivi yksi\n', 'Rivi kaksi\n', 'Rivi kolme\n']
with open('esimerkki.txt', 'w') as tiedosto:
    tiedosto.writelines(rivit)

# Tiedoston lukeminen riveittäin
with open('esimerkki.txt', 'r') as tiedosto:
    for rivi in tiedosto:
        print(rivi, end='')
```
Output:
```
Rivi yksi
Rivi kaksi
Rivi kolme
```

## Deep Dive - Syväsukellus:
Historiallisesti tekstiedostojen kirjoittaminen on ollut tietojenkäsittelyn perustaito. Vaihtoehtoja tekstiedostoille ovat muun muassa binääritiedostot, JSON- ja XML-tiedostot, jotka soveltuvat kompleksisemman datan käsittelyyn. Python hyödyntää alhaalla tason tiedostojen käsittelyyn 'io' -moduulia, mutta tarjoaa 'open' -funktion helppoon käyttöön. 'With'-avainsanan käyttö tiedoston käsittelyssä taas varmistaa, että tiedosto suljetaan automaattisesti, jolloin resursseja ei tuhlaannu.

## See Also - Katso Myös:
- Virallinen Python-dokumentaatio tiedostojen käsittelystä: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- W3Schools Python-tiedostojen käsittelyn tutoriaali: https://www.w3schools.com/python/python_file_handling.asp
- Pythonin 'io' -moduulin dokumentaatio tarjoaa syvällistä tietoa tiedostojen käsittelyyn: https://docs.python.org/3/library/io.html
