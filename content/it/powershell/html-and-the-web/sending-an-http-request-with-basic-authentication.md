---
date: 2024-01-20 18:02:18.264751-07:00
description: "How to: _Output: (dipende dalla risposta dell'API/service, mostrer\xE0\
  \ i dati recuperati o un messaggio di stato)_."
lastmod: '2024-04-05T22:38:43.138734-06:00'
model: gpt-4-1106-preview
summary: "_Output: (dipende dalla risposta dell'API/service, mostrer\xE0 i dati recuperati\
  \ o un messaggio di stato)_."
title: Inviare una richiesta http con autenticazione di base
weight: 45
---

## How to:
```PowerShell
# Definisci le credenziali
$User = 'utente'
$Password = 'password'
$Pair = "$($User):$($Password)"

# Codifica le credenziali in Base64
$EncodedCredentials = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($Pair))

# Prepara l'header per l'autenticazione di base
$Headers = @{
    Authorization = "Basic $EncodedCredentials"
}

# Invia la richiesta GET con le credenziali
$Response = Invoke-RestMethod -Uri 'http://example.com/api' -Method Get -Headers $Headers

# Visualizza il risultato
$Response
```
_Output: (dipende dalla risposta dell'API/service, mostrerà i dati recuperati o un messaggio di stato)_

## Deep Dive:
L'autenticazione HTTP di base è in giro da anni, semplice ma meno sicura rispetto ad alternative più moderne come l'Oauth2.0. Devi solo codificare username e password in Base64 e includerlo nell'header come mostrato sopra. Alternativamente, potresti usare un modulo HTTP di PowerShell come `Invoke-WebRequest` o anche un client HTTP avanzato come curl. Ricorda, usa l'autenticazione di base solo su connessioni sicure (HTTPS) e considera alternative più robuste se la sicurezza è una priorità.

## See Also:
- [Microsoft Docs - Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
