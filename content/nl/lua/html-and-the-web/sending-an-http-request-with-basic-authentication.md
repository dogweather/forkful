---
title:                "Een HTTP-verzoek verzenden met basisauthenticatie"
aliases:
- /nl/lua/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-28T22:08:11.295197-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden met basisauthenticatie"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/lua/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een HTTP-verzoek verzenden met basisverificatie is wanneer je een oproep doet naar een webserver, inclusief een gebruikersnaam en wachtwoord voor toegang. Programmeurs doen dit om te interageren met webservices die gebruikersverificatie vereisen voordat ze gegevens of diensten verstrekken.

## Hoe te:

Lua heeft geen ingebouwde HTTP-ondersteuning, dus je hebt een externe bibliotheek nodig zoals `socket.http` van LuaSocket of `http.request` van de `http`-bibliotheek als je Lua 5.3+ gebruikt. Voor basisauthenticatie encodeer je de inloggegevens en voeg je deze toe in de aanvraagkop.

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")
local mime = require("mime")

-- Jouw inloggegevens
local username = "Aladdin"
local password = "openSesame"
local credentials = mime.b64(username .. ":" .. password)

-- Opzet van het verzoek
local response_body = {}
local res, code, response_headers = http.request{
    url = "http://example.com/data",
    method = "GET",
    headers = {
        ["Authorization"] = "Basic " .. credentials
    },
    sink = ltn12.sink.table(response_body)
}

-- Resultaat uitvoeren
if code == 200 then
    print(table.concat(response_body))
else
    print("Fout: " .. (res or code))
end
```

## Diepgaande duik

HTTP-basisverificatie is een methode voor een HTTP-gebruikersagent om een gebruikersnaam en wachtwoord te verstrekken bij het doen van een aanvraag. Het is vroeg in de geschiedenis van het web uitgevonden, wordt breed ondersteund, maar is niet erg veilig; inloggegevens worden alleen in base64 gecodeerd, niet versleuteld.

Alternatieven omvatten Digest-verificatie, OAuth en API-sleutels – die allemaal sterkere beveiliging bieden. Basisauthenticatie wordt vaak gebruikt voor het scripten van snelle tests, interne hulpmiddelen of waar het transport is beveiligd via HTTPS.

Om basisverificatie in Lua te implementeren, bouw je meestal een string die de gebruikersnaam en het wachtwoord combineert gescheiden door een dubbele punt, waarna je die string codeert met base64. Deze gecodeerde string wordt verzonden in de `Authorization`-kop van je HTTP-aanvraag.

De flexibele aard van Lua betekent dat je keuzes hebt met betrekking tot bibliotheken om HTTP en base64-codering te verwerken. LuaSocket is al lang de go-to voor netwerkoperaties, hoewel nieuwere versies van Lua alternatieven introduceren zoals de `http`-bibliotheek of `CURL`-bindingen voor complexere taken.

## Zie ook

- LuaSocket-documentatie: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec voor HTTPS-ondersteuning: https://github.com/brunoos/luasec/wiki
- Een introductie tot HTTP-verificatie: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- RFC 2617 – HTTP-verificatie: Basis- en Digesttoegangsverificatie: https://tools.ietf.org/html/rfc2617
