---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Nykyisen päivämäärän saaminen ohjelmoinnissa tarkoittaa sitä, että saat työkalut aikaleiman lisäämiseen tietueisiin tai ajan seurantaan reaaliaikaisesti. Se on olennainen osa monia toimintoja, kuten lokiin kirjoittamista, tehtävien ajoitusta ja datan aikasarjojen hallintaa.

## Näin tehdään:

Tässä on Elixir-ohjelmointiesimerkki, joka näyttää kuinka saada nykyinen päivämäärä:

```Elixir
iex> date = Date.utc_today()
~D[2021-11-11]
```

Tämä esimerkki palauttaa nykyisen päivämäärän UTC-muodossa.

## Syvällisemmin:

**Historiallinen yhteys**: Nykyisen päivämäärän hankinta on peräisin päivistä, jolloin tietokonejärjestelmien oli seurattava aikaa ja päivämääriä. Elixir periytti tämän ominaisuuden Erlangilta, sen perustalta.

**Vaihtoehdot**: Jos tarvitset kellonajan lisäksi päivämäärän, voit käyttää `DateTime.utc_now/0`-funktiota:

```Elixir
iex> datetime = DateTime.utc_now()
~U[2021-11-11T08:44:50.044476Z]
```
Nämä funktiot palauttavat tietueen nykyisestä päivämäärästä ja ajasta UTC-muodossa.

**Toteutuksen yksityiskohdat**: Elixir käyttää `:erlang.localtime/0`-funktiota saatuaan päivämäärän ja kellonajan. 

## Katso myös:

[Nykyisen päivämäärän ohjeet Elixir-langissa](https://elixir-lang.org/getting-started/io-and-the-file-system.html#getting-date-and-time)

Lisää tietoa siitä, kuinka käsitellä päivämääriä Elixirissä, löytyy tästä opetusohjelma: [Päivämäärät ja ajat Elixirissä](https://www.learnwithjason.dev/blog/working-with-dates-and-times-in-elixir).