---
title:                "Työskentely csv-tiedostojen kanssa"
html_title:           "Python: Työskentely csv-tiedostojen kanssa"
simple_title:         "Työskentely csv-tiedostojen kanssa"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

CSV-tiedostot ovat erittäin yleisiä tietokannoissa ja taulukoissa, joten on tärkeää osata työskennellä niiden kanssa. Pythonilla tämä on helppoa ja nopeaa!

## Miten

CSV-tiedostojen lukeminen ja kirjoittaminen Pythonissa on hyvin suoraviivaista. Käytämme siihen built-in csv -kirjastoa ja sen avulla voimme käsitellä taulukoita helposti.

```Python
import csv

# Luemme CSV-tiedoston ja tallennamme sen muuttujaan
with open('data.csv', 'r') as csv_file:
    csv_reader = csv.reader(csv_file)

    # Käymme läpi jokaisen rivin tiedostossa
    for row in csv_reader:
        # Tulostamme rivin sisällön
        print(row)
```

```Python
import csv

# Luomme uuden CSV-tiedoston ja tallennamme siihen tietoja
with open('uusi_tiedosto.csv', 'w', newline='') as csv_file:
    csv_writer = csv.writer(csv_file)

    # Lisäämme rivejä tiedostoon
    csv_writer.writerow(['Otsikko 1', 'Otsikko 2', 'Otsikko 3'])
    csv_writer.writerow(['Data 1', 'Data 2', 'Data 3'])
```

### Deep Dive

CSV-tiedostoissa on toisinaan haasteita, kuten erilaisten desimaalipisteiden käyttäminen. Tätä ongelmaa ratkaistaan usein käyttämällä `decimal`-kirjastoa ja määrittämällä `delimiter` parametriksi haluttu desimaalierotin.

Voimme myös käyttää `csv.DictReader` ja `csv.DictWriter` -funktioita, jotka antavat meille mahdollisuuden käsitellä CSV-tiedostoja sanakirjojen avulla.

## Katso myös

- [Python built-in csv dokumentaatio](https://docs.python.org/3/library/csv.html)
- [Python decimal dokumentaatio](https://docs.python.org/3/library/decimal.html)
- [Django's CSV käyttöohje](https://docs.djangoproject.com/en/3.0/howto/outputting-csv/)