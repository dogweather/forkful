---
date: 2024-01-20 18:01:34.892666-07:00
description: "Hur g\xF6r man: Historiskt sett introducerades grundl\xE4ggande autentisering\
  \ i HTTP/1.0 och har varit en enkel metod f\xF6r att styra \xE5tkomst sedan dess.\u2026"
lastmod: '2024-04-05T21:53:39.675242-06:00'
model: gpt-4-1106-preview
summary: "Historiskt sett introducerades grundl\xE4ggande autentisering i HTTP/1.0\
  \ och har varit en enkel metod f\xF6r att styra \xE5tkomst sedan dess."
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
weight: 45
---

## Hur gör man:
```Fish Shell
# Skapa ett kodat värde för autentisering
set -l credentials (echo -n 'anvandarnamn:losenord' | base64)

# Skicka en GET-begäran med basic autentisering
curl -H "Authorization: Basic $credentials" https://exempel.se/api/data

# Exempel på output
{"status":"ok","data": [ ... ]}
```

## Fördjupning
Historiskt sett introducerades grundläggande autentisering i HTTP/1.0 och har varit en enkel metod för att styra åtkomst sedan dess. Alternativ till denna inkluderar tokens, OAuth, och andra, mer säkra autentiseringstekniker som inte exponerar användaruppgifter lika öppet. När du använder grundläggande autentisering i Fish Shell är det viktigt att förstå att informationen inte är krypterad, vilket gör det olämpligt för känsliga eller produktionsmiljöer om den inte kombineras med HTTPS. Det är också viktigt att undvika att spara känslig information direkt i skript.

## Se även
- Fish Shell Dokumentation: https://fishshell.com/docs/current/index.html
- cURL Manual: https://curl.se/docs/manpage.html
- HTTP Basic Authentication Standard: https://tools.ietf.org/html/rfc7617
- Base64 Kodning: https://en.wikipedia.org/wiki/Base64
