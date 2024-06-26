---
date: 2024-01-26 04:21:14.001050-07:00
description: "Kuinka: Elmiss\xE4 ei ole sis\xE4\xE4nrakennettua TOML-j\xE4sent\xE4\
  j\xE4\xE4, mutta voit tehd\xE4 yhteisty\xF6t\xE4 JavaScriptin kanssa tai k\xE4ytt\xE4\
  \xE4 yhteis\xF6n pakettia. N\xE4in saatat\u2026"
lastmod: '2024-03-13T22:44:56.512412-06:00'
model: gpt-4-0125-preview
summary: "Elmiss\xE4 ei ole sis\xE4\xE4nrakennettua TOML-j\xE4sent\xE4j\xE4\xE4, mutta\
  \ voit tehd\xE4 yhteisty\xF6t\xE4 JavaScriptin kanssa tai k\xE4ytt\xE4\xE4 yhteis\xF6\
  n pakettia."
title: "Ty\xF6skentely TOML:n kanssa"
weight: 39
---

## Kuinka:
Elmissä ei ole sisäänrakennettua TOML-jäsentäjää, mutta voit tehdä yhteistyötä JavaScriptin kanssa tai käyttää yhteisön pakettia. Näin saatat jäsentää TOML-tietoja käyttäen hypoteettista `elm-toml` pakettia:

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

Tiettyjen arvojen dekoodaukseen:

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

Esimerkkituloste `port`-arvolle voisi olla `Ok 8080`, jos dekoodaus onnistuu.

## Syväsukellus
Tom Preston-Werner, GitHubin perustajajäsen, loi TOML:n yksinkertaiseksi kieleksi konfiguraatiotiedostoille. Se kilpailee YAML:n ja JSON:n kanssa; TOML:n syntaksi tähtää molempien maailmojen parhaisiin puoliin keskittyen siihen, että ihmiset voivat helposti lukea ja kirjoittaa sitä.

Elmissä TOML:n käsittelyyn tarvitaan yleensä JavaScript-yhteistyötä, mikä voi olla hieman hankalaa. Onneksi Elm-yhteisö on kekseliäs, ja olemassa on useita kolmannen osapuolen paketteja. Hypoteettinen `elm-toml` paketti käyttäisi todennäköisesti Elmin `Port`-toimintoa puhuakseen JavaScriptin TOML-jäsentäjälle tai toteuttaisi jäsentämisen suoraan Elmssä.

Suurin este Elmssä on, että se tyypittää kaiken staattisesti, joten sinun on kirjoitettava räätälöityjä dekoodereita käsittelemään erilaisia tietorakenteita TOML:ssä, mikä voi olla hieman sanallista mutta lisää turvallisuutta.

## Katso Myös
Saat tietoja ja lisätietoja itse TOML:stä, tutustu [TOML](https://toml.io).
Jos etsit käytännönläheistä lähestymistapaa Elmiin ja JavaScript-yhteistyöhön, aloita virallisesta oppaasta: [Elm Ports](https://guide.elm-lang.org/interop/ports.html).
Yhteisöpaketit tai osallistumisen osalta selaa [Elm Packages](https://package.elm-lang.org/).
