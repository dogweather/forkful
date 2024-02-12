---
title:                "Een HTTP-verzoek verzenden met basisauthenticatie"
aliases: - /nl/bash/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-28T22:07:42.186121-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden met basisauthenticatie"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het versturen van een HTTP-verzoek met basisauthenticatie houdt in dat een gebruikersnaam en wachtwoord worden verzonden om de identiteit van een gebruiker te bevestigen. Programmeurs doen dit om toegang te krijgen tot beperkte bronnen op een server, waardoor een zeker niveau van beveiliging wordt gewaarborgd.

## Hoe te:

Laten we onze handen vuilmaken met wat code. We zullen `curl` gebruiken, een gangbare opdrachtregeltool. Vervang `gebruikersnaam:wachtwoord` door je inloggegevens en `http://voorbeeld.com/bron` door je doel-URL.

```Bash
curl -u gebruikersnaam:wachtwoord http://voorbeeld.com/bron
```

Of codeer je inloggegevens van tevoren in base64 en gebruik ze op deze manier:

```Bash
# Inloggegevens coderen
inloggegevens=$(echo -n gebruikersnaam:wachtwoord | base64)

# Het verzoek verzenden
curl -H "Authorization: Basic $inloggegevens" http://voorbeeld.com/bron
```

Voorbeelduitvoer voor een succesvol verzoek kan er als volgt uitzien:

```Bash
{
  "data": "Enige beperkte info",
  "message": "Toegang verleend"
}
```

## Diepere Duik

Historisch gezien maakt basisauthenticatie al sinds de vroege dagen deel uit van HTTP, maar het is niet zonder gebreken - voornamelijk de kwetsbaarheid als het niet wordt gebruikt over een veilig kanaal zoals HTTPS.

Alternatieven zijn onder andere OAuth, wat veiliger is en een fijnmazigere controle biedt over wat er wordt benaderd. Een andere optie is de Digest-authenticatie, die gehashte inloggegevens verzendt in plaats van platte tekst.

Wat betreft de werking, wanneer je basisauthenticatie inloggegevens verzendt, worden deze in de HTTP-header opgenomen gecodeerd in Base64. Het is geen encryptie, dus als je geen HTTPS gebruikt, kan iedereen die het verzoek onderschept het gemakkelijk decoderen. Het gebruik van HTTPS beveiligt de verzending en versleutelt alles tussen de client en de server.

## Zie Ook

- cURL officiële documentatie: https://curl.haxx.se/docs/manpage.html
- HTTP-authenticatie: Basis- en Digest-toegangsauthenticatie (IETF RFC 7617): https://tools.ietf.org/html/rfc7617
- Introductie van OAuth: https://oauth.net/2/introduction/
