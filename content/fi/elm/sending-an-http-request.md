---
title:                "Elm: Http-pyyntöjen lähettäminen"
simple_title:         "Http-pyyntöjen lähettäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

### Miksi

Miksi ihmiset tekevät HTTP-pyyntöjä Elm-ohjelmointikielellä? Yksi syy on, että ne mahdollistavat kommunikoinnin tiettyjen verkkosoitteen kanssa ja saada vastauksen tietokoneelle, jolloin voidaan esimerkiksi luoda dynaamisia verkkosivuja.

### Kuinka

```Elm
import Http
import Json.Decode exposing (..)

type alias User =
  {
    id: Int,
    name: String,
    username: String,
    email: String
  }

getUser: Int -> Cmd Msg
getUser id =
  let
    url = "https://jsonplaceholder.typicode.com/users/" ++ String.fromInt id
    decoder = Json.Decode.map User
    request = Http.get url decoder
  in
    Http.toTask request |> Task.perform HttpErrorMsg GotUser

GotUser: Http.Error User -> Msg
GotUser result =
  case result of
    Http.Error msg ->
      GotHttpError msg

    Http.Ok user ->
      GotUserSuccess user
```

Koodinpätkässä näkyy, miten HTTP-pyyntö voidaan tehdä Elm-ohjelmointikielellä. Tässä tapauksessa halutaan hakea käyttäjätietoja JSON-muodossa verkkosivulta. Koodiin tuodaan `Http`-moduuli, joka mahdollistaa HTTP-pyyntöjen tekemisen ja `Json.Decode`-moduuli, jolla haetaan JSON-tiedostosta haluttua dataa. Funktio `getUser` ottaa parametrina käyttäjän tunnisteen ja muodostaa sitten URL-osoitteen JSON-tiedostoon. `decoder`-muuttuja määrittelee, miten JSON-tiedostosta haetut tiedot tulkitaan ja tallennetaan `User`-tietotyyppiin. Lopuksi `Http.toTask` muuttaa HTTP-pyynnön tehtäväksi, joka suoritetaan tietokoneelle. `Task.perform` määrittelee, mitä tehdään pyynnön onnistuessa tai epäonnistuessa. Esimerkiksi `GotUserSuccess`-viesti lähetetään, jos pyyntö onnistuu ja siinä välitetään palautettu käyttäjätieto. Koodissa käytetään myös `case`-rakennetta käsittelyä varten, joka tarkoittaa, että viestin vastaanottaminen tarkistetaan jäsen kerrallaan.

### Syväsyvennys

HTTP-pyyntöjen tekeminen on tärkeä osa monia verkkosovelluksia. Elm-ohjelmointikielellä on helppo ja turvallinen tapa tehdä näitä pyyntöjä. Tällä tavalla vältetään mahdolliset virheet tai tietoturvariskit, joita voi esiintyä esimerkiksi JavaScript-koodia käytettäessä.

### Katso myös

- [Elm-ohjelmointikielen virallinen dokumentaatio](https://guide.elm-lang.org)
- [HTTP-pyyntöjen tekeminen Elm-ohjelmointikielessä](https://github.com/elm/http)
- [Esimerkkejä erilaisista HTTP-pyynnöistä Elm-ohjelmointikielessä](https://dev.to/jeroenw/elm-http-cookbook-2a0a)