---
title:                "Lähettäminen http-pyyntö"
html_title:           "Elm: Lähettäminen http-pyyntö"
simple_title:         "Lähettäminen http-pyyntö"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit lähettää HTTP-pyynnön? Ehkä tarvitset tietoa ulkoisesta lähteestä tai haluat lähettää tietoa palvelimelle. Tärkeintä on, että HTTP-pyynnön avulla voit kommunikoida eri järjestelmien ja palveluiden välillä.

## Kuinka

Aloitetaan yksinkertaisella esimerkillä, jossa lähetämme GET-pyynnön GitHubin avoimeen rajapintaan (API) ja tulostamme vastauksen konsoliin.

```Elm
import Http

type Msg = GotResponse ( Result Http.Error String )

getGithubResponse : Cmd Msg
getGithubResponse =
  Http.get "https://api.github.com/users/elm/repos" (Http.expectString GotResponse)

```

Kun suoritat tämän funktion, se laukaisee HTTP-pyynnön Githubin API:sta ja saa vastauksena listan Elm-repositorioista. Tämän jälkeen voimme käsitellä vastauksen esimerkiksi tulostamalla sen konsoliin.

## Syväsukellus

HTTP-pyynnön lähettämiseen liittyy useita osia, kuten osoitteiden muodostaminen, muuttujien välittäminen ja vastausten käsittely. On tärkeää, että ymmärrät kaikki nämä osat ennen kuin aloitat HTTP-pyynnön tekemisen.

Aluksi meidän tulee tuoda käyttöön Http-nimisestä moduuli. Tämä moduuli tarjoaa meille funktiot lähettämään erilaisia HTTP-pyynnöistä, kuten `get` ja `post`.

Seuraavaksi meidän tulee määrittää `type Msg` -tyyppi, joka määrittelee miten käsittelemme vastauksia. Tässä tapauksessa laitamme `Http.Errorin` ja `Stringin` sisällä `GotResponse` jossa `Http.Error` viittaa mahdollisiin virheisiin, joita voi tapahtua HTTP-pyynnön lähettämisen aikana.

Nyt voimme aloittaa lähettämään pyyntöjä. Ota huomioon, että käyttämämme `Http.get` -funktio hyväksyy kaksi parametria; ensimmäinen on haluttu osoite ja toinen käsittelee vastauksen. Käytämme funktiota `Http.expectString`, joka muodostaa vastauksen `Stringiksi` ja antaa sen parametriksi `GotResponse`-tyyppiseen funktion poikkeustilanteisiin.

## Katso myös

- [HTTP-pyynnön lähetys](https://guide.elm-lang.org/effects/http.html) Elm-käsikirjasta.
- [Elm-rajapintojen käyttöönotto](http://package.elm-lang.org/packages/elm-lang/http/latest/Http) Elm-paketinhallinnasta.