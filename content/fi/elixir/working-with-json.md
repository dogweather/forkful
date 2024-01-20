---
title:                "Työskentely jsonin kanssa"
html_title:           "Elixir: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

JSON (JavaScript Object Notation) on yleisesti käytetty tiedon tallennusmuoto ja sitä käytetään usein tiedonsiirrossa verkon yli. Monet ohjelmoijat käyttävät JSONia helppona ja tehokkaana tapana tallentaa ja lukea tietoja sovelluksissaan.

## Miten:

```Elixir
#Tiedon tallentaminen JSON-muodossa

data = %{nimi: "Jone", ikä: 25}
JSON.encode(data)

# Tulostaa: "{\"nimi\":\"Jone\",\"ikä\":25}"

# Tietojen avaaminen JSON-muodosta

encoded_data = "{\"nimi\":\"Jone\",\"ikä\":25}"
JSON.decode(encoded_data)

# Tulostaa: %{nimi: "Jone", ikä: 25}
```

## Syväsukellus:

JSON kehitettiin alunperin vaihtoehdoksi XML:lle tiedon hallinnassa, ja se tuli suositummaksi sen yksinkertaisuuden ja tehokkuuden takia. Elixirin sisäänrakennettu JSON-moduuli helpottaa JSONin käsittelyä sovelluksissa ja tarjoaa paljon hyödyllisiä toimintoja, kuten tiedon koodaamisen ja dekoodaamisen.

## Katso myös:

- [JSON Home Page](https://www.json.org/)