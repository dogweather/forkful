---
title:                "Senden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "PowerShell: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage mit grundlegender Authentifizierung ist ein häufiges Szenario für Programmierer, die mit Webdiensten arbeiten. Indem Sie eine Anfrage mit grundlegender Authentifizierung senden, können Sie sich als gültiger Benutzer authentifizieren und auf geschützte Ressourcen zugreifen.

## Wie geht's?

Das Senden einer HTTP-Anfrage mit grundlegender Authentifizierung ist mit PowerShell ein Kinderspiel. Sie können einfach das `Invoke-WebRequest` Cmdlet verwenden und die Parameter `-Credential` und `-UseBasicParsing` angeben. Hier ist ein Beispiel:

```PowerShell
$account = Get-Credential
Invoke-WebRequest -Uri https://example.com/api/ -Credential $account -UseBasicParsing
```

Der Benutzer wird aufgefordert, seine Anmeldeinformationen einzugeben, und die Anfrage wird dann mit diesen Informationen authentifiziert. Wenn die Anfrage erfolgreich ist, erhalten Sie eine Antwort wie diese:

```
StatusCode        : 200
StatusDescription : OK
Content           : {"message": "Hello, world!"}
RawContent        : HTTP/1.1 200 OK
                    Content-Length: 25
                    Content-Type: application/json
                    Date: Mon, 01 Nov 2021 00:00:00 GMT
                    Server: nginx
                    Connection: keep-alive

                    {"message": "Hello, world!"}
Forms             : {}
Headers           : {[Content-Length, 25], [Content-Type, application/json], [Date, Mon, 01 Nov 2021 00:00:00 GMT], [Server, nginx]...}
Images            : {}
InputFields       : {}
Links             : {}
ParsedHtml        : {}
RawContentLength  : 25
```

## Tiefer Einblick

- Historischer Kontext: Die grundlegende Authentifizierung war eine der ersten Methoden, die entwickelt wurden, um Benutzer bei der Verwendung von Webdiensten zu authentifizieren. Sie ist jedoch nicht sicher, da sie Passwörter unverschlüsselt überträgt. Es ist daher üblich, sie in Kombination mit einer sicheren Übertragung wie HTTPS zu verwenden.

- Alternativen: Es gibt viele andere Methoden der Benutzerauthentifizierung, die für Webdienste verwendet werden können, wie z.B. die Digest-Authentifizierung oder die Verwendung von API-Schlüsseln.

- Implementierungsdetails: Die grundlegende Authentifizierung arbeitet auf der Ebene des HTTP-Headers, indem sie die Anmeldedaten mit einem Base64-Encoder verschlüsselt, bevor sie übertragen werden. Der Entschlüsselungsalgorithmus ist einfach und daher sollte die grundlegende Authentifizierung nicht als zuverlässige Sicherheitsmaßnahme betrachtet werden.

## Siehe auch

- Die Microsoft-Dokumentation zu [Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
- Ein [Leitfaden](https://www.positronx.io/create-http-basic-authentication-for-web-service-in-powershell/) zum Erstellen einer grundlegenden Authentifizierung in PowerShell