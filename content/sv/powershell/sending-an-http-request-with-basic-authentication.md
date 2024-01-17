---
title:                "Skicka ett http-begäran med grundläggande autentisering"
html_title:           "PowerShell: Skicka ett http-begäran med grundläggande autentisering"
simple_title:         "Skicka ett http-begäran med grundläggande autentisering"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att man skickar en förfrågan till en webbserver som kräver en användarnamn och lösenord för att få tillgång. Programmerare gör detta för att autentisera användare och säkerställa säker kommunikation.

## Så här gör du:
```PowerShell
$credentials = Get-Credential # Skapa en objektsträng med användarnamn och lösenord
$uri = "https://www.example.com/api/v1/" # Definiera adressen till API:et
$authHeader = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes("$($credentials.GetNetworkCredential().UserName):$($credentials.GetNetworkCredential().Password)")) # Skapa autentiseringsheadern
$headers = @{ "Authorization" = "Basic $($authHeader)" } # Lägg till autentiseringsheadern i en hash-tabell
Invoke-RestMethod -Uri $uri -Headers $headers # Skicka begäran
```

## Djupdykning:
Historiskt sett har grundläggande autentisering använts som den mest enkla och grundläggande metoden för autentisering på webben. Det finns dock andra autentiseringsprotokoll som OAuth som ger en högre nivå av säkerhet. Implementeringen av grundläggande autentisering är relativt enkel, men det finns en risk för att lösenord läcker om en tredje part kan få åtkomst till begäran.

## Se även:
https://www.w3.org/Protocols/rfc2617/rfc2617.html - En RFC-dokumentation som beskriver hur grundläggande autentisering fungerar.
https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.security/get-credential?view=powershell-7 - Get-Credential cmdlet som används i exemplet.
https://oauth.net/2/ - Information om OAuth autentiseringsprotokoll.