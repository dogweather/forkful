---
date: 2024-01-20 18:00:10.141822-07:00
description: "How to: Hier ein einfaches Beispiel, wie man eine GET-Anfrage in PowerShell\
  \ sendet und die Antwort erh\xE4lt."
lastmod: '2024-03-13T22:44:54.100618-06:00'
model: gpt-4-1106-preview
summary: "Hier ein einfaches Beispiel, wie man eine GET-Anfrage in PowerShell sendet\
  \ und die Antwort erh\xE4lt."
title: Einen HTTP-Request senden
weight: 44
---

## How to:
Hier ein einfaches Beispiel, wie man eine GET-Anfrage in PowerShell sendet und die Antwort erhält:

```PowerShell
$response = Invoke-RestMethod -Uri 'http://example.com/api/data' -Method 'GET'
Write-Host $response
```
Ausgabe könnte so aussehen:
```
{ "id": 123, "name": "Beispiel Daten" }
```

Zum Senden einer POST-Anfrage mit JSON-Inhalt:

```PowerShell
$body = @{
    id = 123
    name = 'Neuer Eintrag'
}
$headers = @{
    "Content-Type" = "application/json"
}
$json = $body | ConvertTo-Json
$response = Invoke-RestMethod -Uri 'http://example.com/api/data' -Method 'POST' -Body $json -Headers $headers
Write-Host $response
```
Ausgabe:
```
{ "success": true, "message": "Eintrag angelegt." }
```

## Deep Dive:
HTTP-Anfragen sind seit den frühen Tagen des Web die Grundlage der Client-Server-Kommunikation. PowerShell machte dies zunächst mit `Invoke-WebRequest` möglich, später dann mit `Invoke-RestMethod`, welches einfachere JSON- und XML-Manipulation bietet.

Alternativen zu PowerShell's eingebauten Cmdlets schließen das .NET Framework ein, mit `System.Net.Http.HttpClient` für komplexere Szenarien und feingranuläre Kontrolle. Dies kann nützlich sein, wenn man Einstellungen wie Timeouts, Cookies oder fortgeschrittene Authentifizierungsmethoden benötigt.

Das Senden von HTTP-Anfragen mittels `Invoke-RestMethod` hat einige Besonderheiten. Zum Beispiel analysiert es die Antwort und versucht, sie in das am besten passende PowerShell-Objekt zu konvertieren, was das Arbeiten mit den Daten erleichtert. Außerdem kann man mit Parameter `-Credential` einfache Authentifizierung und mit `-Headers` spezifische Header setzen.

## See Also:
- [Invoke-RestMethod Dokumentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [.NET HttpClient Klasse](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
