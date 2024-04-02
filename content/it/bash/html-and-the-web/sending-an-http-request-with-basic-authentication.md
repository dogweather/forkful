---
date: 2024-01-20 18:00:42.618258-07:00
description: "Inviare una richiesta HTTP con autenticazione di base significa mandare\
  \ le credenziali (username e password) in base64 per accedere a risorse protette.\
  \ I\u2026"
lastmod: '2024-03-13T22:44:43.599587-06:00'
model: gpt-4-1106-preview
summary: "Inviare una richiesta HTTP con autenticazione di base significa mandare\
  \ le credenziali (username e password) in base64 per accedere a risorse protette.\
  \ I\u2026"
title: Inviare una richiesta http con autenticazione di base
weight: 45
---

## What & Why?
Inviare una richiesta HTTP con autenticazione di base significa mandare le credenziali (username e password) in base64 per accedere a risorse protette. I programmatori lo fanno per interagire con API che richiedono autenticazione, accedendo a dati e servizi in modo sicuro.

## How to:
Ecco come usare `curl` per inviare una richiesta HTTP con autenticazione di base.

```Bash
# Imposta le credenziali
USER="mario"
PASS="sicura123"

# Invia la richiesta con autenticazione di base usando curl
curl -u $USER:$PASS https://api.esempio.com/dati

# Output di esempio
{"status":"successo","messaggio":"Benvenuto, Mario!"}
```

## Deep Dive
L'autenticazione di base HTTP risale agli inizi del web, incluso nello standard HTTP 1.0 (RFC 1945). Converte username e password in base64, ma non è criptata, quindi è vulnerabile su connessioni non sicure (use HTTPS!). Alternativa più sicura è OAuth, usato per situazioni che richiedono maggiori garanzie di sicurezza. Quando implementi l'autenticazione di base, ricorda di proteggere le credenziali e di usarla solo su connessioni HTTPS.

## See Also
- [cURL Documentation](https://curl.se/docs/manual.html)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Mozilla Developer Network - HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [OAuth 2.0 Protocol](https://oauth.net/2/)
