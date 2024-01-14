---
title:                "Elm: Å sende en http forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http forespørsel med grunnleggende autentisering"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hvorfor

Å sende en HTTP-forespørsel med grunnleggende autentisering er en viktig del av å bygge nettapplikasjoner. Dette lar deg sikre at bare autoriserte brukere har tilgang til bestemte ressurser på nettet. Det er også en standardmetode for autentisering som er enkelt å implementere og bruke.

# Hvordan

```Elm
import Http
import String.Encode

-- Definer URL for forespørsel med grunnleggende autentisering
url : String
url = "https://api.example.com/get-user-info"

-- Definer autentiseringskrav
authHeader : Http.Header
authHeader = Http.header "Authorization" "Basic " ++ String.Encode.encode "username:password"

-- Utfør HTTP-forespørsel
Http.getStringWith authHeader url
    |> Task.perform handleResponse

-- Behandle svar fra server
handleResponse : Http.Result String -> Cmd msg
handleResponse result =
    case result of
        Err error ->
            -- Behandle feil

        Ok response ->
            -- Behandle vellykket svar
```

Eksempel: Hvis vi bruker en URL til en ressurs som krever grunnleggende autentisering, vil denne koden legge til autentiseringsheaderen i forespørselen og utføre den vellykket.

# Deep Dive

Grunnleggende autentisering er en metode som sender autentiseringsinformasjon i en forespørsel ved hjelp av HTTP-headeren "Authorization". Autentiseringsinformasjonen er kryptert ved hjelp av Base64-koding for å sikre det overføres sikkert til serveren. På serverens side blir autentiseringsinformasjonen dekodet og valideres for å gi tilgang til den etterspurte ressursen. Det er også mulig å sende autentiseringsinformasjon i URIen, men dette er mindre sikker og anbefales ikke.

# Se også

- Elm HTTP-pakken: https://package.elm-lang.org/packages/elm/http/latest/
- Dokumentasjon for grunnleggende autentisering: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication 
- Hvordan kryptere og dekryptere token med Base64 i Elm: https://package.elm-lang.org/packages/elm-community/json-extra/latest/Json-Extra-Base64