---
title:                "Inviare una richiesta http con autenticazione di base"
aliases:
- /it/powershell/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:18.264751-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http con autenticazione di base"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Inviare una richiesta HTTP con autenticazione di base significa inserire credenziali utente in una richiesta per accedere a risorse protette. I programmatori lo fanno per interagire con API o servizi web che richiedono sicurezza.

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
