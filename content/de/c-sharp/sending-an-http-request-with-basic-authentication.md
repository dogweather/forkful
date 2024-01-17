---
title:                "Versenden einer http-Anfrage mit Basis-Authentifizierung"
html_title:           "C#: Versenden einer http-Anfrage mit Basis-Authentifizierung"
simple_title:         "Versenden einer http-Anfrage mit Basis-Authentifizierung"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden eines HTTP-Anfragen mit Basisauthentifizierung ist ein Prozess, bei dem ein Entwickler eine HTTP-Anfrage an einen Server schickt und dabei die Basisauthentifizierung verwendet, um seine Identität zu verifizieren. Dies wird häufig verwendet, um auf geschützte Ressourcen zuzugreifen und sicherzustellen, dass nur autorisierte Benutzer darauf zugreifen können.

## Wie geht's?

```C#
// Beispiel für eine HTTP-Anfrage mit Basisauthentifizierung

// Definieren der URL
var url = "https://www.example.com/api/data";

// Erstellen eines WebRequests
var request = (HttpWebRequest)WebRequest.Create(url);

// Setzen der Basisauthentifizierungsheader
var auth = Convert.ToBase64String(Encoding.Default.GetBytes(username + ":" + password));
request.Headers["Authorization"] = "Basic " + auth;

// Senden der Anfrage und Empfangen der Antwort
var response = (HttpWebResponse)request.GetResponse();

// Lesen der Antwort
var data = new StreamReader(response.GetResponseStream()).ReadToEnd();
Console.WriteLine(data);
```

Die Ausgabe könnte beispielsweise so aussehen:

```
{
    "name": "John Doe",
    "age": 30,
    "gender": "male"
}
```

## Tiefere Einblicke

Die Basisauthentifizierung wurde bereits 1996 in der RFC 1945 spezifiziert und ist eine der ältesten Methoden für die Authentifizierung bei HTTP-Anfragen. Eine Alternative zur Basisauthentifizierung ist beispielsweise die Verwendung von OAuth, welches einen sichereren und granulareren Zugriff auf geschützte Ressourcen ermöglicht.

Bei der Implementierung einer HTTP-Anfrage mit Basisauthentifizierung müssen mehrere Dinge beachtet werden. Zum Beispiel sollte die Übertragung des Benutzernamens und Passworts über eine verschlüsselte Verbindung erfolgen, um die Sicherheit zu erhöhen. Zudem sollten die Anfragen und Antworten sorgfältig überprüft werden, um mögliche Sicherheitslücken zu identifizieren.

## Siehe auch

Weitere Informationen zur Basisauthentifizierung und anderen Formen der HTTP-Authentifizierung finden Sie in der offiziellen RFC 1945. Zusätzlich bietet die Microsoft-Dokumentation eine gute Übersicht über die verschiedenen Methoden der Authentifizierung bei HTTP-Anfragen.