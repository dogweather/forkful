---
date: 2024-01-20 18:00:19.920123-07:00
description: Mandare una richiesta HTTP significa chiedere al web qualcosa. I programmatori
  lo fanno per interagire con API, scaricare dati, o altro ancora.
lastmod: '2024-03-13T22:44:43.639784-06:00'
model: gpt-4-1106-preview
summary: Mandare una richiesta HTTP significa chiedere al web qualcosa. I programmatori
  lo fanno per interagire con API, scaricare dati, o altro ancora.
title: Inviare una richiesta http
weight: 44
---

## What & Why?

Mandare una richiesta HTTP significa chiedere al web qualcosa. I programmatori lo fanno per interagire con API, scaricare dati, o altro ancora.

## How to:

Ecco come si invia una richiesta GET con PowerShell:

```PowerShell
$response = Invoke-RestMethod -Uri 'https://api.example.com/data' -Method Get
Write-Output $response
```

Se devi inviare dati, prova una POST:

```PowerShell
$body = @{
    'chiave' = 'valore'
}
$response = Invoke-RestMethod -Uri 'https://api.example.com/submit' -Method Post -Body $body
Write-Output $response
```

Output di esempio:

```
id: 1234, status: 'successo', messaggio: 'Dati ricevuti correttamente.'
```

## Deep Dive

Inizio: PowerShell ha introdotto `Invoke-RestMethod` in v3.0 (2012). Da allora, è lo standard per le richieste web.

Alternative: Prima c'era `Invoke-WebRequest`, più verboso. Altre opzioni includono curl o strumenti .NET.

Dettagli: `Invoke-RestMethod` analizza la risposta JSON o XML. Ci sono parametri per intestazioni HTTP, autenticazione, e altro.

## See Also

- [Invoke-RestMethod Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [About HTTP Requests](https://developer.mozilla.org/docs/Web/HTTP/Methods)
- [PowerShell Scripting Guide](https://docs.microsoft.com/en-us/powershell/scripting/overview)
