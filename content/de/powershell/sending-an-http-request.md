---
title:                "Senden einer http Anfrage"
html_title:           "PowerShell: Senden einer http Anfrage"
simple_title:         "Senden einer http Anfrage"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
Wenn Programmierer eine HTTP-Anfrage senden, handelt es sich um eine Anfrage an einen Webserver, um Daten zu erhalten oder zu senden. Es ist ein grundlegender Teil der Web-Interaktion und ermöglicht es unseren Anwendungen, auf Daten von verschiedenen Quellen zuzugreifen und mit ihnen zu kommunizieren. Dies ist entscheidend bei der Entwicklung von modernen und dynamischen Anwendungen, die ständig mit dem Internet verbunden sind.

## Wie geht das?
Um eine HTTP-Anfrage in PowerShell zu senden, können wir das `Invoke-WebRequest`-Cmdlet verwenden. Hier ist ein Beispiel:

```PowerShell
$response = Invoke-WebRequest -Uri "https://www.example.com/api" -Method Get
$response.Content
```

In diesem Beispiel verwenden wir das Cmdlet, um eine GET-Anfrage an die angegebene URL zu senden. Das Ergebnis wird in der Variablen `$response` gespeichert und wir können auf den Inhalt der Antwort über die `Content`-Eigenschaft zugreifen.

Wir können auch Header, Parameter und andere Details zur Anfrage hinzufügen, um spezifischere Anfragen zu senden. Hier ist ein Beispiel, bei dem wir die HTTP-Basisauthentifizierung nutzen:

```PowerShell
$response = Invoke-WebRequest -Uri "https://www.example.com/api" -Method Get -Headers @{ Authorization = "Basic dXNlcm5hbWU6cGFzc3dvcmQ=" }
```

In diesem Beispiel geben wir den Benutzernamen und das Passwort in der Kodierung für die Basisauthentifizierung an.

## Tiefergehende Informationen
Das `Invoke-WebRequest`-Cmdlet wurde erst im Jahr 2013 eingeführt und ersetzt das ältere `Invoke-WebRequest`-Cmdlet. Es ermöglicht eine einfachere Handhabung von Webanfragen und bietet eine Vielzahl von Optionen, um die Anfragen anzupassen.

Es gibt auch Alternativen, um HTTP-Anfragen in PowerShell zu senden, wie zum Beispiel das .NET-Framework oder externe Bibliotheken wie `RestSharp`. Diese bieten möglicherweise mehr Funktionen oder bessere Leistung, aber `Invoke-WebRequest` ist eine zuverlässige und integrierte Option.

In Bezug auf die Implementierung sendet das Cmdlet tatsächlich eine `HttpRequestMessage` über den .NET `HttpClient`, weshalb es auch die gleichen Optionen bietet, wie z.B. die Verwendung von Proxys oder Zertifikaten.

## Siehe auch
- [Invoke-WebRequest Dokumentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [.NET HttpClient Klasse](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)