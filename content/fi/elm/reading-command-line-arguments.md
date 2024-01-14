---
title:    "Elm: Komennorivirivi suorittaminen argumenttien lukeminen"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan uutta blogipostaustamme, jossa perehdytään Elm-ohjelmointikieleen ja komentoriviparametrien lukemiseen! Tämä opas on erityisesti suunnattu niille, jotka ovat kiinnostuneita kehittämään taitojaan Elmissä ja oppimaan uusia tapoja hyödyntää tätä ohjelmointikieltä. 

## Kuinka tehdä

```Elm
import Platform exposing (worker)
import Html exposing (..)
import CommandLine.Args as CLA

main : Program Never Model Msg
main =
    worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel, CLA.args Cmd.none )

type Model = 
    { arguments : List String 
    , output : String
    }

type Msg = 
    SetArguments (List String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        SetArguments args ->
            ( { model | arguments = args }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ = 
    Sub.none

view : Model -> Html Msg
view model =
    div [] 
    [ h1 [] [ text "Komentoriviparametrit ja Elm" ]
    , p [] [ text "Seuraavaksi esittelemme, kuinka voit lukea komentoriviltä annettuja parametreja Elm-sovelluksessasi." ]
    , ul [] (List.map (\arg -> li [] [ text arg ]) model.arguments)
    ]

defaultModel : Model
defaultModel = { arguments = [], output = "Ei vielä tulostetta." }
```

Yllä oleva koodiesimerkki näyttää, kuinka komentoriviparametreja voidaan lukea Elm-sovelluksessa. Ensimmäisessä rivissä tuodaan käyttöön tarvittavat moduulit, ja funktiossa `init` annetaan tyhjä malli sekä alustetaan komentoriviparametrit käyttäen `CommandLine.Args` -moduulia. Mallissa on myös muuttuja `output` tulostetta varten.

Seuraavaksi määritellään mallin tyyppi ja viestityyppi, sekä funktio `update`, joka käsittelee viestit ja päivittää mallia tarvittaessa. Funktiossa `view` taas määritellään, miten tulostamme parametrit käyttäen `ul` -elementtiä.

Lopuksi määritellään oletusmalli ja näkymä funktiossa `main`, joka käynnistää ohjelman. 

## Syventymistä

Kommentoriviparametrien lukeminen Elm-sovelluksessa voi olla hyödyllistä esimerkiksi jos haluat antaa käyttäjän antaa alkuparametreja sovelluksen suoritukselle. `CommandLine.Args` -moduulissa on muitakin apufunktioita, joilla voi käsitellä parametreja enemmän. 

## Katso myös

- [Elm dokumentaatio](https://guide.elm-lang.org)
- [Elm esimerkkisovelluksia](https://github.com/elm/projects)
- [Elm-yhteisö](https://discourse.elm-lang.org)