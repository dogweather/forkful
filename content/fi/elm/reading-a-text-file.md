---
title:    "Elm: Tiedoston lukeminen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi 

Tervehdys kaikille! Tiedän että monille lukijoihin innostaa oppia uusi ohjelmointikieli. Tänään keskitymme yhteen suosikkeihin - Elm. Tässä blogikirjoituksessa aiomme käsitellä, miksi sinun kannattaisi lukea tekstitystiedostoja Elm-ohjelmointikielellä ja miten se tehdään.

## Miten

Elmillä on useita tapoja lukea tiedostoja. Yksi yleisimmistä tavoista on käyttää "elm/file" -moduulia. Se tarjoaa tarvittavat toiminnot tiedostojen avaamiseen ja käsittelyyn.

```
import File
import File2 as File


tiedostoTuloste = File.open "tiedosto.txt" File.Read 
case tiedostoTuloste of 
 File.
  Datei.DesireSuccess muodostaa success  
  tiedosto successi 
     tiedostoSelin <-Eine.DeFile (folioGantt tty kieli) Nothing 

  Datei.DesireError error-ilmoitus 
    error <| toString error-ilmoitus 
```

Koodi avaa tiedoston "tiedosto.txt" ja palauttaa tuloksen. Jos tiedoston avaaminen onnistuu, "success" -mutantti saadaan ja tiedosto luetaan "tiedostoSelain" -muuttujaan. Jos tiedoston avaamisessa ilmenee virhe, "error" -muuttujassa on virheilmoitus.

## Syventävä tarkastelu

Voit myös käyttää "elm/http"-moduulia tiedostojen lukemiseen HTTP-pyynnöillä. Tämä on hyödyllinen, jos haluat hakea tiedoston suoraan verkosta.

```
get "http://www.example.com/tiedosto.txt"
    |> Task.attempt ParseResponse
```

Tässä koodiesimerkissä käytetään "elm/http"-moduulia pyyntöön tiedostosta "tiedosto.txt" ja vastaus välitetään "ParseResponse" -funktiolle.

## Katso myös

Lisätietoja tiedostojen lukemisesta Elm-ohjelmointikielellä löydät seuraavista linkeistä:

- Elm tiedoston dokumentaatio: https://package.elm-lang.org/packages/elm/file/latest/File
- Elm http dokumentaatio: https://package.elm-lang.org/packages/elm/http/latest/Http