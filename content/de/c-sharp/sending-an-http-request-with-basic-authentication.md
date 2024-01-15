---
title:                "Senden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "C#: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt die Mühe machen, eine HTTP-Anfrage mit grundlegender Authentifizierung zu senden? Nun, in der heutigen digitalen Welt, in der sensible Informationen ständig online übertragen werden, ist es wichtig, dass diese Informationen geschützt werden. Mit der grundlegenden Authentifizierung können wir sicherstellen, dass nur autorisierte Benutzer auf bestimmte Ressourcen zugreifen können.

## How To

Um eine HTTP-Anfrage mit grundlegender Authentifizierung zu senden, müssen wir zunächst die entsprechenden Bibliotheken importieren. Anschließend können wir die Anfrage mit Hilfe von Code durchführen.

```C#
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;

var client = new HttpClient(); // erstellt eine neue HTTP-Client-Instanz

var credentials = Encoding.ASCII.GetBytes("Benutzername:Passwort"); // konvertiert Benutzername und Passwort in den erforderlichen Base64-String

client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", Convert.ToBase64String(credentials)); // fügt die Basic-Authentifizierung zur Anfrage hinzu

var response = await client.GetAsync("https://beispiel.com/api/resource"); // sendet die Anfrage an die angegebene URL

Console.WriteLine(await response.Content.ReadAsStringAsync()); // gibt die Antwort der Anfrage aus
```

Die Ausgabe sollte eine erfolgreiche Anfrage mit dem Inhalt der Ressource enthalten.

## Deep Dive

Die grundlegende Authentifizierung ist eine der ältesten und am häufigsten verwendeten Methoden für die Authentifizierung in HTTP-Anfragen. Sie basiert auf dem Base64-String, der aus dem Benutzernamen und Passwort erstellt wird. Es ist wichtig zu beachten, dass es sich bei der grundlegenden Authentifizierung nicht um eine sichere Methode handelt, da der Base64-String leicht entschlüsselt werden kann. Aus diesem Grund sollte bei der Verwendung von HTTP-Anforderungen mit grundlegender Authentifizierung immer eine sichere Verbindung (HTTPS) verwendet werden, um die Sicherheit der übertragenen Informationen zu gewährleisten.

## Siehe auch

Hier sind einige hilfreiche Ressourcen, die Ihnen eine tiefere Einsicht in das Thema HTTP-Anfragen und grundlegende Authentifizierung geben können:

- [MSDN-Dokumentation von HttpClient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Grundlegende Authentifizierung in der HTTP-Produktion](https://www.rfc-editor.org/rfc/rfc7617.txt)
- [Reaktion des Webservers auf HTTP-Anfragen](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)