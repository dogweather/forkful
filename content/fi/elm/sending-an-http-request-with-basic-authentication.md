---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnin avulla"
html_title:           "Elm: HTTP-pyynnön lähettäminen perusautentikoinnin avulla"
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnin avulla"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi?

Miksi kukaan haluaisi lähettää HTTP-pyyntöä perusautentikoinnilla? Yksinkertainen vastaus on, että perusautentikointi on yksi tapa suojata verkkosivuston tai sovelluksen tietokantaan tallennettuja tietoja varmistamalla, että ne ovat lähtöisin luotettavasta lähteestä.

## Kuinka?

Aloitetaan luomalla yksinkertainen HTTP-pyyntö, jossa käytetään perusautentikointia Elm-ohjelmointikielellä. Käytämme tätä esimerkkiä osoitteenhakusivustoon, joka vaatii kirjautumisen ennen kuin se palauttaa käyttäjän tietoja. 

```Elm
import Http
import Http.BasicAuth as Auth

-- Määritä tarvittavat tiedot
username = "käyttäjä"
password = "salasana"
url = "https://www.example.com/api/user"

-- Luo HTTP-pyyntö käyttäen BasicAuth -moduulia
req = Http.toRequest
    { method = "GET"
    , headers =
        [ Auth.header username password
        ]
    , url = url
    , body = Http.emptyBody
    }

-- Lähetä pyyntö ja tulosta vastaus
Http.send (\_ -> (Auth.authenticate req)) |> Task.attempt handleResponse
 
-- Määritä vastauksen käsittely
handleResponse : Result Http.Error String -> Cmd msg
handleResponse result =
    case result of
        Ok response ->
            "Käyttäjän tiedot: " ++ response |> log

        Err error ->
            "Virhe: " ++ Http.errorToString error |> log
```

Tässä koodissa käytämme `Http.BasicAuth` -moduulia luomaan otsakitiedon, joka sisältää annetun käyttäjänimen ja salasanan. Sitten käytämme `Http.send` -funktiota lähettämään pyynnön ja tulostamme vastauksen `handleResponse` -funktion avulla. Lopuksi käsittelemme vastauksen joko onnistuneena tai virheellisenä ja tulostamme vastaavan viestin.

## Syväsukellus

Perusautentikoinnin käyttäminen HTTP-pyynnöissä on vain yksi tapa varmistaa, että käyttäjän tiedot ovat turvattuja. Samalla tavalla voimme käyttää muita kodin tai muita Autentikoinnin tarjoajia soluun.

Elm tarjoaa myös muita vaihtoehtoja, kuten Web.Sockets, joiden avulla voidaan lähettää ja vastaanottaa tietoja reaaliaikaisesti. Deep Elm -verkkosivusto tarjoaa lisätietoja näistä vaihtoehdoista ja auttaa sinua kehittämään edistynympää toiminnallisuutta käyttäen HTTP-pyyntöjä.

## Katso myös

- [Elm-lang.org - Basic Auth](https://guide.elm-lang.org/webapps/authentication.html)
- [Deep Elm - HTTP](https://deep.elm-lang.org/web/http)
- [Elm Weekly - Resting with Elm](https://elmweekly.nl/issues/51)