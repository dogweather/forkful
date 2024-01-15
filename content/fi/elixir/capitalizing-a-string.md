---
title:                "Merkkijonon ensimmäisen kirjaimen suuriksi kirjaimiksi muuttaminen"
html_title:           "Elixir: Merkkijonon ensimmäisen kirjaimen suuriksi kirjaimiksi muuttaminen"
simple_title:         "Merkkijonon ensimmäisen kirjaimen suuriksi kirjaimiksi muuttaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Merkkijonon isojen kirjainten käyttö voi olla hyödyllistä esimerkiksi tietokantojen hakutoiminnoissa tai luotaessa selkeää käyttöliittymää.

## Näin teet sen

Hyvin yksinkertainen tapa muuttaa merkkijonon kirjaimet isoiksi on käyttää String.upcase/1 -funktiota. Se ottaa argumenttina merkkijonon ja palauttaa uuden merkkijonon, jossa kaikki kirjaimet ovat isoja. Esimerkiksi:
```
Elixir: String.upcase("moi") # "MOI"
```

Mikäli haluat muuttaa vain tietyn alueen merkkijonon kirjaimista isoiksi, voit käyttää String.replace/4 -funktiota. Se ottaa argumentteina merkkijonon, mille alueelle muutokset tehdään, miltä osalta muutokset tehdään ja millaisiksi muutokset tehdään. Esimerkiksi:
```
Elixir: String.replace("Moi maailma!", "maailma", &String.upcase/1, overlap: true) # "Moi MAAILMA!"
```

## Syväsukellus

Merkkijonon isojen kirjainten käyttöön liittyy usein myös esiintymien laskeminen ja järjestäminen. Tämä onnistuu käyttämällä String.split/2 ja Enum.sort/1 -funktioita. Esimerkiksi:
```
Elixir: "Moi Moi Maailma!" 
|> String.split(" ") 
|> Enum.sort 
# ["Maailma!", "Moi", "Moi"]
```

Osana merkkijonon muokkaamista, voi myös olla tarpeellista poistaa merkkijonosta välimerkit tai muut erikoismerkit. Tämä onnistuu käyttämällä String.replace/4 -funktiota ja regular expression -syntaksia. Esimerkiksi:
```
Elixir: String.replace("Terve, maailma!", ~r/[[:punct:]]/, "") # "Terve maailma"
```

## Katso myös

- [String -moduuli Elixirin dokumentaatiossa](https://hexdocs.pm/elixir/String.html)
- [Regular Expression -opas Elixirille](https://elixir-lang.org/getting-started/regex.html)
- [Koodiesimerkkejä Elixirin verkkosivuilla](https://elixir-lang.org/getting-started/introduction.html)