---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "PowerShell: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

In poche parole, l'invio di una richiesta HTTP con autenticazione di base è un processo attraverso il quale un programma invia una richiesta a un server web inserendo un nome utente e una password per autenticarsi. I programmatori lo fanno per accedere a risorse e dati protetti tramite l'autenticazione di base.

## Come fare:

Questo è un esempio semplice di come fare una richiesta HTTP con autenticazione di base in PowerShell:

```PowerShell
# Impostare gli URL del server e delle credenziali
$server = "https://www.example.com"
$username = "utente"
$password = ConvertTo-SecureString "password" -AsPlainText -Force

# Creare un oggetto delle credenziali
$cred = New-Object System.Management.Automation.PSCredential($username, $password)

# Fare la richiesta HTTP al server con l'autenticazione di base
Invoke-RestMethod -Uri $server -Method Get -Credential $cred
```

Ecco il risultato che dovreste ottenere:

```PowerShell
StatusCode        : 200
StatusDescription : OK
Content           : Some content
RawContent        : HTTP/1.1 200 OK
                    Date: Mon, 01 Mar 2021 00:00:00 GMT
                    Content-Type: text/html; charset=UTF-8
                    Content-Length: 14
                    Connection: Keep-Alive
```

## Approfondimento:

L'autenticazione di base HTTP è uno dei metodi più semplici di autenticazione, introdotto nel 1999 come parte delle specifiche di HTTP/1.0. Sebbene sia facile da implementare, è considerata meno sicura rispetto ad altri metodi di autenticazione.

Ci sono alternative più sicure all'autenticazione di base, come l'autenticazione tramite token o OAuth. Tuttavia, l'autenticazione di base è ancora utilizzata in alcune situazioni in cui è necessaria una soluzione veloce e semplice.

Per inviare una richiesta HTTP con autenticazione di base, si utilizza il cmdlet `Invoke-RestMethod` di PowerShell, che rende il processo molto semplice e diretto.

## Vedi anche:

- [Documentazione di Microsoft su Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1)
- [Autenticazione di base HTTP su Wikipedia](https://it.wikipedia.org/wiki/Autenticazione_di_base_http)
- [Articolo di CodeProject su come utilizzare l'autenticazione di base in PowerShell](https://www.codeproject.com/Articles/1179317/PowerShell-Basic-Authentication)