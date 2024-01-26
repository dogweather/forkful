---
title:                "CSV-tiedostojen käsittely"
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Comma-Separated Values) on yksinkertainen tiedostoformaatti, joka sisältää taulukkomuotoista dataa. Ohjelmoijat käyttävät CSV:tä, koska se on yleisesti tuettu, helppolukuinen ja -kirjoitettavissa oleva datanvaihdon formaatti.

## How to:
### CSV-tiedoston lukeminen
```Python
import csv

# Avaan CSV-tiedoston lukemista varten
with open('esimerkki.csv', mode='r', encoding='utf-8') as tiedosto:
    csv_lukija = csv.reader(tiedosto)
    
    for rivi in csv_lukija:
        print(rivi)

# Output esimerkki:
# ['sarake1', 'sarake2', 'sarake3']
# ['data1', 'data2', 'data3']
```

### CSV-tiedoston kirjoittaminen
```Python
import csv

# Määritän datan
data = [['sarake1', 'sarake2', 'sarake3'], ['data1', 'data2', 'data3']]

# Kirjoitan dataa CSV-tiedostoon
with open('esimerkki_uusi.csv', mode='w', newline='', encoding='utf-8') as tiedosto:
    csv_kirjoittaja = csv.writer(tiedosto)
    
    for rivi in data:
        csv_kirjoittaja.writerow(rivi)

# Output tiedostossa:
# sarake1,sarake2,sarake3
# data1,data2,data3
```

## Deep Dive
CSV:n historialliset juuret ovat 1970-luvulla, jolloin se soveltui yksinkertaisiin datan esitysmuotoihin. Nykyään on olemassa monipuolisempia vaihtoehtoja, kuten JSON tai XML, mutta CSV pysyy suosiossa sen yksinkertaisuuden ja laajan tukensa ansiosta. Kun työstetään CSV-tiedostoja Pythonissa, tärkeää on huomioida oikeanlainen merkistökoodaus (usein utf-8), rivinvaihtojen käsittely ja datan muoto (esimerkiksi desimaalierottimen käsittely).

## See Also
- Python `csv`-moduuli: [https://docs.python.org/3/library/csv.html](https://docs.python.org/3/library/csv.html)
- CSV:n Wikipedia-artikkeli: [https://fi.wikipedia.org/wiki/CSV](https://fi.wikipedia.org/wiki/CSV)
- CSV:n ja muiden formaattien vertailu: [https://realpython.com/python-csv/](https://realpython.com/python-csv/)
