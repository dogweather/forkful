---
title:                "Kahden päivämäärän vertailu"
html_title:           "Python: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärien vertaaminen on prosessi, jossa tarkastellaan kahden päivämäärän välistä suhdetta ja määritetään, kumpi niistä on aiempi tai myöhempi. Tämä on tärkeää ohjelmoinnissa esimerkiksi tapahtumien järjestämisen, aikaleimojen tarkistamisen ja raportointitietojen käsittelyn kannalta.

## Kuinka?

Vertaamme päivämääriä usein käyttämällä Pythonin "datetime" -kirjastoa. Alla olevassa esimerkissä näemme kuinka voimme verrata kahta eri päivämäärää ja tulostaa tiedon siitä, kumpi niistä on aiempi.

```Python
import datetime

päivä1 = datetime.date(2021, 5, 1)
päivä2 = datetime.date(2021, 5, 10)

if päivä1 < päivä2:
  print("Päivä 1 on aiempi kuin päivä 2")
else:
  print("Päivä 2 on aiempi kuin päivä 1")
```

Tulostus:

`Päivä 1 on aiempi kuin päivä 2`

## Syventävä sukellus

Päivämäärien vertaamista on tehty jo vuosikymmenien ajan ja erilaisia menetelmiä on kehitetty. Suosittu tapa on käyttää Excelin "DATEDIF" -toimintoa, joka laskee päivämäärien välisenä päivinä, kuukausina tai vuosina. Tämä toiminto vastaa osittain Pythonin "timedelta" -objektia. Myös SQL-tietokannassa on omat toiminnot päivämäärien vertailuun.

Pythonissa on myös mahdollista käyttää muita moduuleja, kuten "dateutil" ja "arrow", jotka tarjoavat lisämahdollisuuksia päivämäärien vertailuun ja käsittelyyn.

## Katso myös

[Lyhyt esimerkki Pythonin "datetime" -kirjaston käytöstä](https://docs.python.org/3/library/datetime.html)

[Excelin "DATEDIF" -toiminnon käyttöohjeet](https://support.microsoft.com/en-us/office/datedif-function-25dba1a4-2812-480b-84dd-8b32a451b35c)

[Dateutil-kirjaston dokumentaatio](https://dateutil.readthedocs.io/en/stable/)

[Arrow-kirjaston GitHub-sivut](https://github.com/arrow-py/arrow)