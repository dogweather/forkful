---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage mit Basisauthentifizierung ist eine übliche Methode, um sich bei einer Website oder einem Webdienst anzumelden. Programmierer verwenden diese Methode oft, um Benutzern einen sicheren Zugang zu einer Website zu ermöglichen.

## So geht's:

Es ist einfach, in PowerShell eine HTTP-Anfrage mit Basisauthentifizierung zu erstellen. Beachten Sie die folgenden Beispiele:

```PowerShell
# Zuerst erstellen wir die Anmeldedaten
$User = 'Benutzername'
$Pass = 'Passwort' | ConvertTo-SecureString -AsPlainText -Force
$Credential = New-Object -TypeName System.Management.Automation.PSCredential -ArgumentList $User, $Pass

# Dann Instanziieren wir die WebClient-Klasse und fügen die Anmeldedaten hinzu
$WebClient = New-Object System.Net.WebClient
$WebClient.Credentials = $Credential

# Jetzt erstellen und senden wir die Anfrage
$Result = $WebClient.DownloadString('http://example.com')

# Lassen Sie uns das Resultat betrachten
$Result
```

## Vertiefung:

Das HTTP Basic Authentication Protokoll wurde erstmals 1996 im HTTP/1.0 Spezifikations-Dokument RFC 1945 vorgestellt. Es ist ein einfacher Mechanismus zur Authentifizierung, bei dem ein Benutzername und ein Passwort über HTTP-Kopfzeilen gesendet werden. 

Alternativen zur Basisauthentifizierung sind Digest-Authentifizierung, Formular-basierte Authentifizierung, OAuth und OpenID, die zusätzliche Sicherheitsfunktionen bieten können.

Die Details der Implementierung der Basisauthentifizierung in PowerShell beinhalten die Verwendung der .NET WebClient-Klasse und die `DownloadString`-Methode, die den Webinhalt als Zeichenkette herunterlädt.

## Weiterführende Informationen:

- [MDN Web Docs: HTTP Basic Access Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [RFC 1945: HTTP/1.0 Spezifikationsdokument](https://tools.ietf.org/html/rfc1945)
- [PowerShell-Dokumentation: Die WebClient-Klasse](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=netframework-4.8)