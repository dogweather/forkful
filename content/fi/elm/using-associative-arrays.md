---
title:                "Assosiatiivisten taulukoiden käyttö"
aliases:
- fi/elm/using-associative-arrays.md
date:                  2024-01-30T19:10:55.674921-07:00
model:                 gpt-4-0125-preview
simple_title:         "Assosiatiivisten taulukoiden käyttö"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Assosiatiiviset taulukot, tai kuten Elm kutsuu niitä, Sanakirjat, kartoittavat avaimet arvoihin tavalla, joka tekee arvojen hakemisesta, lisäämisestä ja poistosta erittäin nopeaa. Ne ovat sinun valintasi, kun tarvitset pitää kirjaa asioista ilman tiukkaa järjestystä, kuten käyttäjäasetukset tai varastolistat.

## Kuinka:

Elmissä työskentelet Sanakirjojen kanssa `Dict` moduulissa, joten sukellamme nopeasti esimerkkiin:

```Elm
import Dict exposing (Dict)

-- Alustetaan sanakirja, jossa on String-avaimet ja Int-arvot
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- Arvon lisääminen tai päivittäminen
updatedDict = Dict.insert "grape" 10 exampleDict

-- Arvon hakeminen (huomaa Maybe-tyyppi, koska avainta ei välttämättä ole)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- Avain-arvo -parin poistaminen
finalDict = Dict.remove "banana" updatedDict

-- Muunnetaan sanakirja takaisin listaksi
dictToList = Dict.toList finalDict
```

Esimerkkitulostus, kun näytetään `dictToList`:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

Tämä osoittaa perusoperaatiot: luomisen, päivittämisen, käyttämisen ja iteroimisen Sanakirjan yli.

## Syväsukellus

Elmin Sanakirjat sisäisesti käyttävät rakennetta, joka tunnetaan nimellä AVL-puu - tyyppi itseään tasapainottava binäärihakupuu. Tämä valinta löytää tasapainon varmistamalla, että operaatiot kuten insert, get ja remove ovat hyvän suorituskyvyn omaavia (logaritminen aikavaativuus) ja ylläpitävät yksinkertaisuutta datan käsittelyssä.

Elmin `Dict` vahvuuksista huolimatta, se ei ole kaikille kokoelmatyypeille sopiva ratkaisu. Järjestetyille kokoelmille tai sellaisille, jotka täytyy iteroida peräkkäin, Lista tai Taulukko saattaisi olla sopivampi. Lisäksi, kun työskennellään kiinteän joukon tunnettujen avainten kanssa, käyttäjän määrittelemät tyypit (Elmin versio enumeraatioista) voivat tarjota enemmän tyyppiturvallisuutta ja selvempiä aikomuksia koodissasi.

Elmin ekosysteemissä `Dict` tarjoaa luotettavan tavan hallita avain-arvo -pareja kokoelmia, joissa avaimet ovat uniikkeja ja järjestyksellä ei ole merkitystä. Vaikka uudempia tai kehittyneempiä rakenteita voi ilmestyä, `Dict` moduuli pysyy perustyökaluna Elm-ohjelmoijan työkalupakissa sen yksinkertaisuuden ja tehokkuuden vuoksi assosiatiivisten taulukoiden käsittelyssä.
