---
title:                "Sending en http-forespørsel med grunnleggende autentisering"
html_title:           "Elm: Sending en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http-forespørsel med grunnleggende autentisering"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle du ønske å sende en HTTP-forespørsel med basic authentication i Elm? Det korte svaret er at dette er en enkel og sikker måte å verifisere brukeridentiteten din når du kommuniserer med en API-server.

## Hvordan

La oss starte med å importere pakken som lar oss lage HTTP-forespørsler i Elm:

```Elm
import Http
```

Nå kan vi definere en funksjon som sender en GET-forespørsel med basic authentication:

```Elm
sendRequest : Http.Request
sendRequest =
  Http.get
    { url = "https://api.example.com"
    , headers =
        [ ("Authorization", "Basic username:password") ]
    , body = Http.emptyBody
    }

```

I koden over ser vi at vi setter URL-en til API-serveren vi ønsker å kommunisere med, og legger til en spesiell header med brukerens brukernavn og passord i basic authentication format. Deretter sender vi forespørselen ved å bruke `Http.get`-funksjonen og ta imot svaret som en `Http.Request`-verdi.

For å håndtere responsen, kan vi bruke en `case`-setning:

```Elm
case sendRequest of
  Http.BadUrl url ->
    -- Håndter en ugyldig URL-feil
  Http.Timeout ->
    -- Forespørselen tok for lang tid å få svar på
  Http.NetworkError ->
    -- Noe gikk galt med nettverket
  Http.BadStatus status ->
    -- Håndter en ugyldig statuskode fra serveren
  Http.GoodStatus response ->
    -- Bruk `response` for å få tilgang til dataene fra serveren
```

Vi kan også legge til en funksjon for å konvertere serverresponsen til et mer leselig format, for eksempel JSON:

```Elm
decodeResponse : Http.Response String -> String
decodeResponse response =
  case response of
    Http.BadResponse description ->
      "-- Kunne ikke dekode serverresponsen: " ++ description
    Http.GoodResponse body ->
      body
```

## Dypdykk

Det er verdt å merke seg at basic authentication ikke er den mest sikre metoden for verifisering, da brukernavn og passord sendes i klartekst. Det anbefales å bruke mer avansert autentisering, for eksempel OAuth, hvis mulig. Likevel, hvis du trenger å bruke basic authentication, er det alltid bedre å kryptere kommunikasjonen ved å bruke HTTPS-protokollen.

En annen ting å huske på er at du må sørge for at brukernavnet og passordet er riktig kodet før du sender dem i forespørselen. I Elm kan du bruke `Base64.encode`-funksjonen for å encode dem på riktig måte.

## Se også

* [Offisiell Elm-dokumentasjon](https://guide.elm-lang.org)
* [GitHub-prosjekt for Elm HTTP-pakken](https://github.com/elm-lang/http)