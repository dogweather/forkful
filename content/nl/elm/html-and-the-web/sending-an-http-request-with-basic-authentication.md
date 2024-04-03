---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:12.555375-07:00
description: "Hoe: Elm maakt HTTP-verzoeken met behulp van het `Http`-pakket. Om basisauthenticatie\
  \ toe te voegen, codeer je de inloggegevens en neem je deze op in de\u2026"
lastmod: '2024-03-13T22:44:50.724406-06:00'
model: gpt-4-0125-preview
summary: Elm maakt HTTP-verzoeken met behulp van het `Http`-pakket.
title: Een HTTP-verzoek verzenden met basisauthenticatie
weight: 45
---

## Hoe:
Elm maakt HTTP-verzoeken met behulp van het `Http`-pakket. Om basisauthenticatie toe te voegen, codeer je de inloggegevens en neem je deze op in de verzoekheaders.

```Elm
importeer Http
importeer Base64

type alias Model = { ... }
type Msg = HttpRequestCompleted (Resultaat Http.Fout String)

-- Gebruikersnaam en wachtwoord coderen
basicAuthHeader : String -> String -> Http.Header
basicAuthHeader gebruikersnaam wachtwoord =
    laat
        inloggegevens = gebruikersnaam ++ ":" ++ wachtwoord
        gecodeerdeInloggegevens = Base64.codeer inloggegevens
    in
    Http.header "Autorisatie" ("Basis " ++ gecodeerdeInloggegevens)

-- Het HTTP-verzoek doen
sendRequestWithBasicAuth : Cmd Msg
sendRequestWithBasicAuth =
    laat
        url = "https://example.com/protected/resource"
        verzoek =
            Http.verzoek
                { methode = "GET"
                , headers = [ basicAuthHeader "mijnGebruikersnaam" "mijnWachtwoord" ]
                , url = url
                , lichaam = Http.emptyBody
                , verwacht = Http.expectString (HttpRequestCompleted)
                , timeout = Niets
                , tracker = Niets
                }
    in
    Http.stuur HttpRequestCompleted verzoek
```

Wanneer de bovenstaande functie wordt aangeroepen, zal Elm een GET-verzoek uitvoeren naar de opgegeven URL met de Autorisatieheader ingesteld op de gecodeerde gebruikersnaam en wachtwoord.

## Diepere Duik
Elm's benadering van HTTP-verzoeken weerspiegelt de algehele filosofie van de taal: veilig, makkelijk te onderhouden en begrijpelijk. Het `Http`-pakket incorporeert verzoeken op een manier die omgaat met de Elm-architectuur.

Basisauthenticatie is zo oud als het web zelf, deel van de originele HTTP-specificatie (RFC 7617). Het is eenvoudig maar niet erg veilig aangezien de inloggegevens alleen base64-gecodeerd zijn, niet versleuteld. Daarom is het cruciaal om HTTPS te gebruiken om de transmissie te coderen.

Alternatieven voor basisauthenticatie zijn onder andere OAuth, tokens zoals JWT of API-sleutels, elk met een verhoogde complexiteit en verbeterde beveiliging. Elm ondersteunt ook deze methoden, maar vereist vaak extra pakketten of aangepaste codeerders en decodeerders.

## Zie Ook
- Elm's Officiële `Http`-pakket documentatie: [package.elm-lang.org/packages/elm/http/latest](https://package.elm-lang.org/packages/elm/http/latest)
- Elm's `Base64`-pakket bron: [package.elm-lang.org/packages/truqu/elm-base64/latest](https://package.elm-lang.org/packages/truqu/elm-base64/latest)
- RFC 7617, Het 'Basis' HTTP-authenticatieschema: [tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
