---
title:                "Verkkosivun lataaminen"
html_title:           "Elm: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä ja mikä?
Lataaminen verkkosivulle tarkoittaa sivun tiedon tallentamista tietokoneelle tai muuhun laitteeseen sitä varten että sivustoa voidaan käyttää offline-tilassa tai että sen sisältöä voidaan käsitellä ohjelmallisesti. Ohjelmoijat voivat ladata verkkosivuja esimerkiksi tietojen keräämiseksi tai niiden käsittelyä varten.

## Miten tehdä se:
```Elm
import Http
import Json.Decode as Json

type alias WebPage = 
    { url : String
    , content : String
    }

downloadWebPage : String -> Cmd Msg
downloadWebPage url = 
    Http.get 
        { url = url
        , expect = Http.expectString Response
        }

type Msg 
    = Response (Result Http.Error String)

update : Msg -> WebPage -> WebPage
update msg currentPage = 
    case msg of 
        Response res -> 
            case res of 
                Ok content -> 
                    { currentPage | content = content }
                Err error -> 
                    currentPage 
```

## Syvempi sukellus:
Lataaminen verkkosivulle on ollut tärkeä osa web-kehitystä alusta lähtien. Monet ohjelmointikielet tarjoavat työkaluja verkkosivujen lataamiseen, mutta Elm tarjoaa turvallisen ja käyttäjäystävällisen tavan tehdä sitä. Vaihtoehtoja lataamiseen ovat esimerkiksi JavaScript-bibliotekit ja selaimen sisäänrakennettu XMLHttpRequest-ominaisuus. Elmissä lataaminen tapahtuu käyttämällä sisäänrakennettua `Http` -moduulia, joka tarjoaa yksinkertaisen rajapinnan verkkosivujen lataamiselle. Tarkemmat tiedot `Http` -moduulin toteutuksesta ja käytöstä löytyvät Elm dokumentaatiosta.

## Katso myös:
- Elm dokumentaatio [Http -moduulista](https://package.elm-lang.org/packages/elm/http/latest/)
- [HttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest) selaimen sisäänrakennettu ominaisuus 
- Erilaisia JavaScript-kirjastoja verkkosivujen lataamiseen kuten [axios](https://github.com/axios/axios) ja [fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)