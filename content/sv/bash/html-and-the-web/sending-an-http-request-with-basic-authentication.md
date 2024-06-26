---
date: 2024-01-20 18:00:46.339040-07:00
description: "Hur man g\xF6r: I b\xF6rjan av webben var Basic-auth ett enkelt s\xE4\
  tt att skydda resurser. Information kodas i Base64 men \xE4r inte krypterad, vilket\
  \ g\xF6r det\u2026"
lastmod: '2024-04-05T22:50:52.387369-06:00'
model: gpt-4-1106-preview
summary: "I b\xF6rjan av webben var Basic-auth ett enkelt s\xE4tt att skydda resurser."
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
weight: 45
---

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
