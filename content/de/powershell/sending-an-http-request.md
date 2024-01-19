---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein HTTP-Anfrage senden bedeutet, eine Datenanforderung an einen Webserver zu senden. Programmierer tun dies, um Informationen von Servern abzurufen, bestimmte Aktionen auf dem Server auszuführen oder Server zu überwachen.

## So geht's:

Um eine HTTP-Anfrage in PowerShell zu senden, verwendet man Invoke-WebRequest oder Invoke-RestMethod. Die einfachste Anfrage wäre ein GET-Anforderung:

```PowerShell
$antwort = Invoke-WebRequest -Uri 'http://example.com'
```
Ausgabe wäre:

```PowerShell
StatusCode        : 200
StatusDescription : OK
Content           : {...}
RawContent        : HTTP/1.1 200 OK
                    Content-Length: 606
Headers           : {[Content-Length, 606], [Content-Type, text/html; charset=UTF-8], ...}
Images            : {}
InputFields       : {}
Links             : {...}
...
```
## Tieftauchen

Die Verwendung von HTTP-Anfragen in Skriptsprachen stammt aus der Anfangszeit des Internets, als Daten hauptsächlich über HTML-Formulare ausgetauscht wurden. Ein Invoke-Befehl in PowerShell ist eine moderne Möglichkeit, HTTP-Anfragen zu senden.

Alternativen zu Invoke-WebRequest oder Invoke-RestMethod in PowerShell sind .NET Klassen wie System.Net.WebRequest oder System.Net.Http.HttpClient.

Beachten Sie, dass wenn Sie mit API-Endpunkten arbeiten, der Server manchmal Header oder Cookies benötigt. Hier ist ein Beispiel, wie man einen benutzerdefinierten Header sendet:

```PowerShell
$headers = New-Object "System.Collections.Generic.Dictionary[[String],[String]]"
$headers.Add("Authorization", "Bearer dein_token")

$antwort = Invoke-RestMethod -Uri 'http://example.com' -Headers $headers
```
## Siehe auch

- Mehr zu Invoke-WebRequest: https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1
- Mehr zu Invoke-RestMethod: https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1
- Mehr zu .NET HttpClient: https://docs.microsoft.com/de-de/dotnet/api/system.net.http.httpclient?view=net-5.0