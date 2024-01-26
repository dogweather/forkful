---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
date:                  2024-01-20T18:01:34.892666-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att du överför inloggningsuppgifter (användarnamn och lösenord) i klartext, kodat med base64, i en HTTP-header. Programmerare gör detta för att enkelt bekräfta användaridentitet vid enkel API-åtkomst eller webbtjänster.

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
