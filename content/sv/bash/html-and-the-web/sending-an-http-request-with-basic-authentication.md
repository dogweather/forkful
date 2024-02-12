---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
date:                  2024-01-20T18:00:46.339040-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att man skickar användarnamn och lösenord över nätet för att bevisa sin identitet. Programmerare gör det för att komma åt skyddade resurser på webbservrar.

## Hur man gör:
```Bash
# Skapa en variabel med användarnamn och lösenord
USER_PASS="användarnamn:lösenord"

# Använd curl för att skicka en HTTP GET-begäran med Basic-auth
curl -u $USER_PASS http://exempel.se/data

# För POST-begäran, använda -X POST och -d för data
curl -u $USER_PASS -X POST -d '{"nyckel":"värde"}' http://exempel.se/data
```
Exempel på svar:
```Bash
{"svar":"Du har lyckats autentisera och få tillgång till skyddad data."}
```

## Fördjupning
I början av webben var Basic-auth ett enkelt sätt att skydda resurser. Information kodas i Base64 men är inte krypterad, vilket gör det osäkert över osäkra nätverk. Idag brukar man ofta använda starkare autentiseringsmetoder som OAuth. För HTTP-begäran finns alternativ som `wget` eller programmeringsspråkens inbyggda bibliotek (ex. Python's `requests`), men `curl` är utbrett för sin flexibilitet och enkelhet.

## Se även:
- curl's officiella dokumentation: https://curl.se/docs/
- HTTP Basic Authentication Standard: https://tools.ietf.org/html/rfc7617
- OAuth 2.0: https://oauth.net/2/
