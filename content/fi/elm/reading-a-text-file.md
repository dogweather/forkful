---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstitiedoston lukeminen tarkoittaa prosessia, jossa ohjelma avaa ja tulkitsee tietyssä tiedostossa olevan tiedon. Tätä tehdään, jotta ladatut, ulkoiset tai käyttäjän toimittamat tiedot voidaan sisällyttää sovelluksen toimintaan.

## Kuinka:

Elm (kuten monien muidenkin selaimessa ajettavien kielten) ei tue suoraa tiedoston luvun toiminnallisuutta turvallisuussyistä. Kuitenkin seuraava esimerkkikoodi, esittää miten selaimeen ladattu tiedosto voidaan lukea käyttäen `File.Reader` APIa:

```Elm
import File exposing (File)
import File.Select as Select
import File.Reader as Reader

type Msg
    = Selected (Maybe File)
    | Loaded (Result String String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Selected maybeFile ->
            case maybeFile of
                Nothing ->
                    ( model, Cmd.none )

                Just file ->
                    ( model, Reader.string file |> Task.attempt Loaded )

        Loaded result ->
            case result of
                Err error ->
                    ( model, Cmd.none )

                Ok contents ->
                    ( { model | content = contents }
                    , Cmd.none
                    )
```

## Syvempi sukellus

Vaikka Elm:ää ei ole tarkoitettu suoraan tiedostojen lukemiseen, on se hyvin yhdistettävissä muihin koodikirjastoihin, jotka tarjoavat tämän toiminnallisuuden. Historiallisessa kontekstissa tiedostojen lukeminen on keskeinen osa ohjelmointia; se mahdollistaa tiedon säilyttämisen ja hakemisen.

Vaihtoehtoisesti, tiedot voidaan hakea verkosta HTTP-pyynnöllä tai Käyttöliittymästä (UI) käyttäjän syötteestä. Elm tarjoaa tavan tehdä molemmat nämä toiminnot.

Elm:n tiedostojen lukemisen toteutus perustuu JavaScriptin File API:in, mutta se käsittelee sen sisäisiä yksityiskohtia omassa funktionaalisessa paradigmassaan.

## Katso myös

Lisätietoja varten, katso seuraavia linkkejä:

- Elm:n dokumentaatio: https://package.elm-lang.org/packages/elm/file/latest/
- JavaScriptin File API: https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications
- File Reader API: https://developer.mozilla.org/en-US/docs/Web/API/FileReader