---
title:                "Työskentely csv-tiedostojen kanssa"
html_title:           "Elixir: Työskentely csv-tiedostojen kanssa"
simple_title:         "Työskentely csv-tiedostojen kanssa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
CSV-tiedostojen (Comma-Separated Values) käsittely on tärkeä osa ohjelmointia, sillä usein data tallennetaan CSV-muodossa. CSV-muoto mahdollistaa tiedon tallentamisen helposti riveinä ja sarakkeina.

## Miten?
Elixir tarjoaa kätevän tavan käsitellä CSV-tiedostoja `CSV`-moduulin avulla. Alla oleva esimerkkikoodi näyttää, kuinka voit lukea CSV-tiedoston ja muuttaa sen listaksi:

```Elixir
data = File.stream!("tiedosto.csv")
|> CSV.decode()
|> Enum.to_list()
```

Tämän jälkeen `data`-muuttujaan tallentuu lista, jonka jokainen alkio edustaa yhtä riviä CSV-tiedostossa. Voit käyttää `Enum`-moduulia käsitelläksesi listaa edelleen ja tehdäksesi esimerkiksi laskutoimituksia tiedon kanssa.

CSV-tiedoston luominen on yhtä helppoa, sillä voit käyttää `CSV`-moduulia myös tiedoston kirjoittamiseen. Alla oleva koodi tallentaa `data`-muuttujan sisältämän listan uuteen CSV-tiedostoon:

```Elixir
File.write!("uusi_tiedosto.csv", CSV.encode(data))
```

## Syväsukellus
CSV-muoto kehitettiin jo vuonna 1972 ja se on edelleen suosittu tapa tallentaa taulukkomuotoista dataa. Sen yksinkertaisuus ja helppokäyttöisyys tekevät siitä suositun ohjelmointikielistä riippumatta.

Elixirin lisäksi on olemassa muitakin työkaluja CSV-tiedostojen käsittelyyn, kuten esimerkiksi Pythonin `csv`-moduuli tai Rubyyn sisäänrakennettu `CSV`-luokka. Näiden lisäksi on myös olemassa monia erilaisia kirjastoja, jotka tarjoavat erilaisia toiminnallisuuksia CSV-tiedostojen käsittelyyn.

Elixirin `CSV`-moduuli on erityisen hyödyllinen, sillä se käsittelee dataa tehokkaasti ja tarjoaa monia hyödyllisiä toimintoja, kuten automaattisen rivitäyttökyvyn ja mahdollisuuden työskennellä tiedostoa streamina eli käsitellä dataa rivi kerrallaan.

## Katso myös
- [Elixirin virallinen dokumentaatio CSV-moduulille](https://hexdocs.pm/csv/)
- [CSV-muodon historia](https://en.wikipedia.org/wiki/Comma-separated_values#History)