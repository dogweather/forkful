---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Skicka en HTTP-begäran med grundläggande autentisering innebär att du ber en server om data med ett användarnamn och lösenord. Programmerare gör detta för att skydda data, kommunicera säkert och följa behörighetsbestämmelser.

## Hur man:
```
# Importera nödvändig guide
Import-Module Microsoft.PowerShell.Utility;

# Användarnamn och lösenord
$user = 'ditt_användarnamn'
$pass = 'ditt_lösenord'

# Skapa autentisering
$pair = "$($user):$($pass)"
$encodedCreds = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($pair))
$basicAuthValue = "Basic $encodedCreds"

# Skapa headers
$headers = @{
    Authorization = $basicAuthValue
}

# URL till webbplatsen du begär data från
$url = 'https://ditt_websida.com'

# Skicka begäran
$response = Invoke-RestMethod -Uri $url -Headers $headers -Method Get

# Skriv ut svaret
Write-Output $response
``` 

## Djupdykning

Historiskt sett är grundläggande autentisering en förenklad metod för autentisering över HTTP-protokollet. Även om denna metod är vanlig finns det alternativ som erbjuder bättre säkerhet, som OAuth, som förlitar sig på tokens istället för lösenord.

När du implementerar HTTP-begäran med grundläggande autentisering i PowerShell, bör du vara medveten om att lösenordet skickas över nätverket i klartext. Använd alltid HTTPS för att skydda dina data när du överför det på nätet.

## Se också

- [PowerShell Dokumentation](https://docs.microsoft.com/sv-se/powershell/)
- [Grundläggande autentisering](https://developer.mozilla.org/sv-SE/docs/Web/HTTP/Authentication)