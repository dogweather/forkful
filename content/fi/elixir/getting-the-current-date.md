---
title:                "Nykyisen päivämäärän saaminen"
html_title:           "Elixir: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Nykyisen päivämäärän hakeminen on prosessi, jossa ohjelma hakee ja tallentaa nykyisen päivämäärän ja kellonajan tietokoneen järjestelmäkellolta. Tätä tarvitaan usein ohjelmoinnissa esimerkiksi tapahtumien ajoittamisessa tai arkistojen järjestämisessä.

# Kuinka:

``` Elixir
Time.utc_now()
```
## Tuloste:

``` Elixir
# ⟨DateTime.utc_ #<Year: 2021, month: 10, day: 25, hour: 15, minute: 30, second: 45, microsecond: {0, 500, 0}, timezone: Etc/UTC⟩
```

## Syventävä tutkimus:

### Historiallinen konteksti:

Ennen tietokoneita päivämäärät laskettiin käsin tai käytettiin mekaanisia laitteita, kuten aurinkokelloja. Tietokoneiden myötä päivämäärän hakeminen on tullut helpommaksi ja nopeammaksi.

### Vaihtoehtoisia tapoja:

On olemassa useita tapoja hakea nykyinen päivämäärä eri ohjelmointikielillä. Elixirissä käytetään Time-moduulia, mutta esimerkiksi Pythonissa käytetään datetime-moduulia.

### Toteutus:

Nykyisen päivämäärän hakeminen perustuu tietokoneen järjestelmäkellon tarjoamiin tiedonkeruu- ja tallennusominaisuuksiin. Ohjelma lukee järjestelmäkellon tallentaman päivämäärän ja ajan, ja muuntaa sen sitten haluttuun muotoon.

# Katso myös:

Lisätietoa Elixirin Time-mallista: https://hexdocs.pm/elixir/DateTime.html

Lisätietoa päivämäärän hakemisesta muilla ohjelmointikielillä: https://www.w3schools.com/jsref/jsref_getfullyear.asp