---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Inviare una richiesta HTTP con autenticazione di base è un modo per comunicare con i server web che richiedono un nome utente e una password. I programmatori lo fanno per accedere a risorse protette online, come database o aree private.

## Come si fa
Ecco un esempio di script PowerShell che invia una richiesta HTTP con autenticazione di base:

```PowerShell
$cred = New-Object System.Management.Automation.PSCredential ("username", 
    (ConvertTo-SecureString "password" -AsPlainText -Force)
)
$response = Invoke-RestMethod -Uri "http://your-url/api" -Credential $cred
```
Il risultato sarà la risposta HTTP del server. Sei autenticato!

## Approfondimento
Lo standard dell'autenticazione di base HTTP è stato definito per la prima volta nel 1996 da RFC 1945. Nonostante la sua età, è ancora ampiamente usato per la sua semplicità. Tuttavia, tieni in mente che le credenziali trasmesse non sono crittografate, quindi non è sicuro usarlo su reti non sicure.

Un'alternativa più sicura è l'autenticazione Digest, che crittografa le credenziali prima di inviarle. Un'altra opzione è utilizzare l'autenticazione a token, come JWT (JSON Web Token).

Dettagli implementativi da notare sono che l'header "Authorization" per l'autenticazione di base è composto dalla parola "Basic" seguita da un nome utente e una password codificati in base64 separati da due punti.

## Vedi anche
- [Guida Microsoft su Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1). 
- [Dettagli RFC 1945](https://tools.ietf.org/html/rfc1945).
- [Guida JWT](https://jwt.io/introduction/).