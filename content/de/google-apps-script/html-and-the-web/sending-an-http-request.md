---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:32.246607-07:00
description: "Das Senden einer HTTP-Anfrage in Google Apps Script bedeutet, programmgesteuert\
  \ eine Anfrage an einen externen Webserver oder eine API zu stellen.\u2026"
lastmod: '2024-02-25T18:49:50.530388-07:00'
model: gpt-4-0125-preview
summary: "Das Senden einer HTTP-Anfrage in Google Apps Script bedeutet, programmgesteuert\
  \ eine Anfrage an einen externen Webserver oder eine API zu stellen.\u2026"
title: Eine HTTP-Anfrage senden
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage in Google Apps Script bedeutet, programmgesteuert eine Anfrage an einen externen Webserver oder eine API zu stellen. Programmierer tun dies, um Daten von Webdiensten abzurufen oder an sie zu senden, und integrieren so ein weites Reich von Webressourcen und Funktionalitäten direkt in ihre Google Apps Script-Projekte.

## Wie:

In Google Apps Script ist die primäre Methode zum Senden einer HTTP-Anfrage die Verwendung des `UrlFetchApp`-Dienstes. Dieser Dienst bietet Methoden, um HTTP-GET- und POST-Anfragen zu machen. Hier ist ein einfaches Beispiel für das Ausführen einer GET-Anfrage, um JSON-Daten abzurufen:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

Für eine POST-Anfrage, die üblicherweise verwendet wird, um Daten an einen Server zu senden, müssen Sie weitere Details im Optionsparameter angeben:

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'value1',
    key2: 'value2'
  };
  
  var options = {
    'method': 'post',
    'contentType': 'application/json',
    // Das JavaScript-Objekt in einen JSON-String umwandeln
    'payload': JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

Diese Snippets zeigen grundlegende Implementierungen von GET- und POST-Anfragen. Die Ausgabe hängt von der API-Antwort ab und kann im Logger von Google Apps Script angesehen werden.

## Tiefer eintauchen

Der `UrlFetchApp`-Dienst von Google Apps Script hat sich seit seiner Einführung erheblich weiterentwickelt und bietet eine nuanciertere Kontrolle über HTTP-Anfragen mit Funktionen wie dem Setzen von Headern, dem Payload und der Handhabung von multipart/form-data für Dateiuploads. Obwohl er ein geradliniges Mittel bietet, externe Webdienste zu integrieren, finden Entwickler, die aus robusteren Backend-Sprachen kommen, seine Funktionalität möglicherweise im Vergleich zu Bibliotheken wie Pythons `requests` oder JavaScripts `fetch` API in Node.js etwas begrenzt.

Eine bemerkenswerte Einschränkung ist das Ausführungszeitlimit für Google Apps Script, das sich auf lang andauernde Anfragen auswirkt. Zudem deckt `UrlFetchApp` zwar eine breite Palette von Anwendungsfällen ab, komplexere Szenarien, die OAuth-Authentifizierung oder die Handhabung sehr großer Payloads beinhalten, erfordern möglicherweise kreative Lösungen oder die Nutzung zusätzlicher Google-Cloud-Ressourcen.

Nichtsdestotrotz bietet `UrlFetchApp` für die meisten Integrationen, mit denen Google Workspace-Entwickler konfrontiert sind – von der Automatisierung der Datenabfrage bis zum Posten von Updates an externe Dienste – ein starkes, zugängliches Werkzeug. Seine Integration in Google Apps Script bedeutet, dass keine externen Bibliotheken oder komplexen Konfigurationen erforderlich sind, wodurch das Ausführen von HTTP-Anfragen innerhalb der Beschränkungen von Google Apps Script relativ unkompliziert ist. Da sich die Landschaft der Web-APIs weiterhin ausdehnt, bleibt `UrlFetchApp` eine kritische Brücke für Google Apps Script-Programme, um mit der Welt jenseits von Googles Ökosystem zu interagieren.
