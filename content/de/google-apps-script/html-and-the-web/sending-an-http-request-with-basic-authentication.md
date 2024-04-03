---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:04.232096-07:00
description: "Das Senden einer HTTP-Anfrage mit Basis-Authentifizierung beinhaltet\
  \ das Kodieren eines Benutzernamens und eines Passworts in einen Anfrage-Header,\
  \ um\u2026"
lastmod: '2024-03-13T22:44:53.333560-06:00'
model: gpt-4-0125-preview
summary: "Das Senden einer HTTP-Anfrage mit Basis-Authentifizierung beinhaltet das\
  \ Kodieren eines Benutzernamens und eines Passworts in einen Anfrage-Header, um\
  \ Zugang zu gesch\xFCtzten Ressourcen zu erhalten."
title: Eine HTTP-Anfrage mit einfacher Authentifizierung senden
weight: 45
---

## Wie:
In Google Apps Script senden Sie eine HTTP-Anfrage mit Basis-Authentifizierung, indem Sie den `UrlFetchApp`-Dienst zusammen mit einem Base64-kodierten Autorisierungs-Header nutzen. Hier ist eine Schritt-für-Schritt-Anleitung:

1. **Anmeldeinformationen kodieren**: Zuerst kodieren Sie Ihren Benutzernamen und Ihr Passwort in Base64. Google Apps Script hat keine native Base64-Kodierungsfunktion für Zeichenketten, daher verwenden Sie dafür Utilities.base64Encode.

```javascript
var username = 'IhrBenutzername';
var password = 'IhrPasswort';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **Anfrageoptionen einrichten**: Mit den kodierten Anmeldeinformationen bereiten Sie das Optionsobjekt für die HTTP-Anfrage vor, einschließlich Methode und Headern.

```javascript
var options = {
  method: 'get', // oder 'post', 'put', je nach Bedarf
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // zusätzliche Optionen wie 'muteHttpExceptions' für die Fehlerbehandlung können hier hinzugefügt werden
};
```

3. **Die Anfrage stellen**: Verwenden Sie die `UrlFetchApp.fetch`-Methode mit der Ziel-URL und dem Optionsobjekt.

```javascript
var url = 'https://example.com/api/resource';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

Die Beispielausgabe bei erfolgreicher Anfrage variiert je nach Antwort der API. Bei einer auf JSON basierenden API könnten Sie etwas wie Folgendes sehen:

```
{"status":"Erfolg","data":"Ressourcendaten hier..."}
```

Stellen Sie sicher, dass Sie mögliche HTTP-Fehler behandeln, indem Sie den Antwortcode überprüfen oder die Option `muteHttpExceptions` für ein kontrollierteres Fehlermanagement verwenden.

## Vertiefung
Das Senden einer HTTP-Anfrage mit Basis-Authentifizierung ist in vielen Programmiersprachen eine Standardmethode für den Zugriff auf webbasierte Ressourcen, die eine Authentifizierung erfordern. Im Kontext von Google Apps Script bietet `UrlFetchApp` eine unkomplizierte Möglichkeit, diese HTTP-Anfragen durchzuführen, einschließlich derer, die eine Authentifizierung erfordern. Die Einbindung von Basis-Anmeldeinformationen in die Anfrage-Header ist eine einfache, aber wirksame Methode, die jedoch Sicherheitsbedenken mit sich bringt, hauptsächlich weil die Anmeldeinformationen im Klartext gesendet werden, nur base64-kodiert, was leicht dekodiert werden kann, wenn es abgefangen wird.

Für verbesserte Sicherheit werden Alternativen wie OAuth 2.0 empfohlen, insbesondere beim Umgang mit sensiblen Daten oder Operationen. Google Apps Script bietet mit der `OAuth2`-Bibliothek integrierte Unterstützung für OAuth 2.0 und erleichtert so den Authentifizierungsprozess gegenüber Diensten, die dieses Protokoll unterstützen.

Trotz seiner Sicherheitsbeschränkungen bleibt die Basis-Authentifizierung für einfache oder interne Anwendungen, die nicht dem breiteren Internet ausgesetzt sind, weit verbreitet. Sie ist einfach zu implementieren, da sie nur eine einzige Anfrage mit ordnungsgemäß gesetzten Headern erfordert, was sie zu einer attraktiven Option für schnelle Integrationen oder für APIs macht, bei denen höhere Sicherheitsmethoden nicht verfügbar sind. Programmierer werden jedoch dazu aufgefordert, die Sicherheitsimplikationen zu berücksichtigen und sicherere Alternativen zu erkunden, wenn diese verfügbar sind.
