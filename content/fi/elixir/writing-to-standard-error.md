---
title:    "Elixir: Kirjoittaminen standardivirheeseen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaa standardivirheeseen?

Kirjoittaminen standardivirheeseen on tärkeä osa Elixir-ohjelmoinnissa. Se voi auttaa löytämään virheitä ja ongelmatilanteita ohjelman suorituksen aikana. Se on myös hyvä tapa varmistaa, että ohjelman suoritus onnistuu ilman ongelmia.

## Miten

Esimerkkejä ja koodilohkoja:

```Elixir
IO.puts "Kirjoitetaan virheviesti standardivirheeseen"
```

```
Kirjoitetaan virheviesti standardivirheeseen
```

Kuten näemme yllä olevassa koodilohkossa, meillä on IO.puts-funktio, jonka avulla voimme kirjoittaa viestin standardivirheeseen. Tämä viesti näkyy terminaalissa tai komentokehotteessa, joka suorittaa ohjelman.

Voimme myös käyttää IO.inspect-funktiota tarkastellaksemme muuttujien arvoja ja kirjoittaa ne standardivirheeseen. Tämä on erityisen hyödyllistä silloin, kun haluamme tietää, mitä tapahtuu ohjelman suorituksen aikana.

```Elixir
nimi = "Maria"
IO.inspect nimi
```

```
Maria
```

## Syvällinen sukellus

Kirjoittaessamme standardivirheeseen, voimme myös käyttää Logger-moduulia. Tämä antaa meille enemmän hallintaa siitä, mihin virheviestit kirjoitetaan ja miten niitä käsitellään.

Esimerkiksi voimme määrittää, että virheviestit kirjoitetaan tiedostoon tai lähetetään eteenpäin toiselle palvelimelle loggausta varten.

```Elixir
Logger.error("Tämä on virheviesti")
```

Tässä tapauksessa virheviesti kirjoitetaan standardivirheeseen käyttämällä Logger-moduulia.

## Katso myös

- [Elixir Dokumentaatio](https://hexdocs.pm/elixir/)
- [Logger-moduulin käyttö](https://elixirschool.com/en/lessons/advanced/logging/)
- [Lue ja kirjoita tiedostoihin Elixirissa](https://elixirschool.com/en/lessons/basics/io/)