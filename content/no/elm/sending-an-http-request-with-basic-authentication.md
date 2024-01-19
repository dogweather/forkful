---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

---
# Send HTTP-forespørsler med grunnleggende autentisering i Elm

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende autentisering betyr å lage en nettforespørsel som inneholder brukerdetaljer (brukernavn og passord) i headeren. Programmerere gjør dette for å få tilgang til beskyttede nettressurser.

## Hvordan?

```Elm
import Http
import Http.Headers as Headers


basicAuth : String -> String -> Http.Header
basicAuth username password =
    let
        credentials =
            username ++ ":" ++ password

        base64 =
            credentials
                |> Http.Base64.encode
                |> Maybe.withDefault ""
    in
    Headers.authorization ("Basic " ++ base64)

main =
    Http.get 
        { url = "https://example.com/protected-data"
        , headers = [ basicAuth "myUsername" "myPassword" ]
        , expect = Http.expectString GotResponse
        }
```

I dette eksempelet vises det hvordan du kan lage en 'basicAuth'-funksjon for å generere en autorisasjonsheader. Denne headeren legges deretter til i HTTP GET-forespørselen.

## Dypdykk

- *Historisk kontekst:* Grunnleggende autentisering er en del av HTTP/1.0-spesifikasjonen, datert tilbake til 1996. 

- *Alternativer:* Selv om grunnleggende autentisering er enkel å implementere, sikrer den overførte data dårlig. Brukere sender passord i klartekst, base64-kodet. Sikrere alternativer inkluderer Digest- og Bearer-autentisering, samt autentiseringsløsninger med OAuth og OpenID.

- *Implementeringsdetaljer:* Elm bruker sin `Http`-pakke for å håndtere HTTP-forespørsler. For grunnleggende autentisering genereres en 'Authorization'-header ved å base64-kode brukernavn og passord.

## Se også

- [Elm Http Package](https://package.elm-lang.org/packages/elm/http/latest/)
- [HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Basic Access Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)

---