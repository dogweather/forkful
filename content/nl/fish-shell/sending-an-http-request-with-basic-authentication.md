---
title:                "Een HTTP-verzoek verzenden met basisauthenticatie"
date:                  2024-01-28T22:07:54.550349-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden met basisauthenticatie"

category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het versturen van een HTTP-verzoek met basisauthenticatie houdt in dat een gebruikersnaam en wachtwoord over het web worden verzonden om toegang te krijgen tot beschermde bronnen. Programmeurs gebruiken dit voor de eenvoud bij het omgaan met API's of diensten die inloggegevens vereisen.

## Hoe te:

In Fish Shell gebruik je `curl` om een HTTP-verzoek met basisauthenticatie te maken. Vervang `username`, `password` en `the_url`:

```Fish Shell
set -x AUTH (echo -n "username:password" | base64)
curl -H "Authorization: Basic $AUTH" the_url
```

Of laat `curl` de codering afhandelen:

```Fish Shell
curl -u username:password the_url
```

Een voorbeelduitvoer ziet er mogelijk als volgt uit:

```Fish Shell
HTTP/1.1 200 OK
Content-Type: application/json
{
  "message": "Succesvol geauthenticeerd."
}
```

## Diepgaande Duik

Basisauthenticatie is deel van het HTTP-protocol en bestaat al sinds de vroege jaren 90. Hoewel het eenvoudig te implementeren is, is het minder veilig omdat inloggegevens alleen base64-gecodeerd zijn, niet versleuteld. HTTPS helpt, maar het is niet onfeilbaar.

Alternatieven zijn onder meer OAuth, dat tokens gebruikt in plaats van inloggegevens, wat zorgt voor extra beveiligingslagen. Voor meer beveiliging, overweeg het gebruik van API-sleutels of JWT (JSON Web Tokens).

Met Fish Shell werken we samen met `curl`, een krachtige tool die verschillende protocollen en authenticatiemethoden ondersteunt. De `-u` vlag is handig, maar vermijd het hardcoderen van inloggegevens; gebruik in plaats daarvan omgevingsvariabelen of configuratiebestanden met de juiste toestemmingen.

## Zie Ook:

- cURL Documentatie: https://curl.se/docs/httpscripting.html
- HTTP Basis Auth RFC: https://tools.ietf.org/html/rfc7617
- Fish Shell Documentatie: https://fishshell.com/docs/current/index.html
- Begrip van JWT: https://jwt.io/introduction/
