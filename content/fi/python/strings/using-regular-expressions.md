---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:21.268634-07:00
description: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) ovat malleja, joita k\xE4ytet\xE4\
  \xE4n merkkiyhdistelmien etsimiseen merkkijonoista. Ohjelmoijat k\xE4ytt\xE4v\xE4\
  t niit\xE4 tekstin\u2026"
lastmod: '2024-03-13T22:44:56.133589-06:00'
model: gpt-4-0125-preview
summary: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) ovat malleja, joita k\xE4ytet\xE4\
  \xE4n merkkiyhdistelmien etsimiseen merkkijonoista. Ohjelmoijat k\xE4ytt\xE4v\xE4\
  t niit\xE4 tekstin\u2026"
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Säännölliset lausekkeet (regex) ovat malleja, joita käytetään merkkiyhdistelmien etsimiseen merkkijonoista. Ohjelmoijat käyttävät niitä tekstin etsimiseen, muokkaamiseen tai manipulointiin määriteltyjen mallien perusteella, mikä tekee niistä välttämättömiä tehtävissä kuten datan validointi, jäsentäminen tai muuntaminen.

## Kuinka:
Pythonissa regexin käyttöön liittyy `re`-moduuli, joka tarjoaa joukon funktioita tekstin käsittelyyn säännöllisten lausekkeiden avulla.

### Perusmallin Etsintä
Jos haluat etsiä mallia merkkijonosta, käytä `re.search()`. Se palauttaa otteluobjektin, kun malli löytyy, muuten `None`.
```python
import re

teksti = "Opettele Python-ohjelmointia"
ottelu = re.search("Python", teksti)
if ottelu:
    print("Malli löytyi!")
else:
    print("Mallia ei löytynyt.")
```
Tuloste:
```
Malli löytyi!
```

### Säännöllisten Lausekkeiden Kokoaminen
Jos samaa mallia käytetään toistuvasti, käännä se ensin `re.compile()`-toiminnolla paremman suorituskyvyn saavuttamiseksi.
```python
malli = re.compile("Python")
ottelu = malli.search("Opettele Python-ohjelmointia")
if ottelu:
    print("Koottu malli löytyi!")
```
Tuloste:
```
Koottu malli löytyi!
```

### Merkkijonojen Jakaminen
Merkkijonon jakamiseksi jokaisessa regex-mallin vastaavuudessa käytä `re.split()`.
```python
tulos = re.split("\s", "Python on kivaa")
print(tulos)
```
Tuloste:
```
['Python', 'on', 'kivaa']
```

### Kaikkien Vastaavuuksien Etsiminen
Kaikkien päällekkäisyyksiä vailla olevien mallien esiintymien etsimiseksi käytä `re.findall()`.
```python
ottelut = re.findall("n", "Python-ohjelmointi")
print(ottelut)
```
Tuloste:
```
['n', 'n']
```

### Tekstin Korvaaminen
Käytä `re.sub()` korvataksesi mallin esiintymät uudella merkkijonolla.
```python
korvattu_teksti = re.sub("kivaa", "mahtavaa", "Python on kivaa")
print(korvattu_teksti)
```
Tuloste:
```
Python on mahtavaa
```

### Kolmannen Osapuolen Kirjastot
Vaikka Pythonin sisäänrakennettu `re`-moduuli on tehokas, tarjoavat kolmannen osapuolen kirjastot, kuten `regex`, lisää ominaisuuksia ja parannettua suorituskykyä. `regex`-kirjaston käyttämiseksi asenna se pip:n kautta (`pip install regex`) ja tuo se koodiisi.

```python
import regex

teksti = "Opiskellaan Python 3.8"
ottelu = regex.search(r"Python\s(\d+\.\d+)", teksti)
if ottelu:
    print(f"Löydetty versio: {ottelu.group(1)}")
```
Tuloste:
```
Löydetty versio: 3.8
```
