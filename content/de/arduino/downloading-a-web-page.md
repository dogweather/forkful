---
title:                "Arduino: Das Herunterladen einer Webseite"
simple_title:         "Das Herunterladen einer Webseite"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Bevor wir uns mit dem Herunterladen von Webseiten in Arduino befassen, stellt sich die Frage: Warum sollte man das überhaupt machen? Nun, es gibt viele Gründe, aber einer der wichtigsten ist die Möglichkeit, Daten aus dem Internet in unsere Projekte zu integrieren. Durch das Herunterladen und Verarbeiten von Webseiteninhalten können wir beispielsweise Echtzeit-Informationen wie Wetterdaten, Nachrichten oder Aktienkurse in unseren Arduino-basierten Geräten anzeigen.

## Wie man eine Webseite herunterlädt

Das Herunterladen einer Webseite in Arduino mag auf den ersten Blick kompliziert erscheinen, aber es gibt eine relativ einfache Möglichkeit, dies zu erreichen. Wir werden die in Arduino eingebaute Ethernet-Bibliothek verwenden, um eine Verbindung zum Internet herzustellen und die benötigten Daten herunterzuladen.

Zunächst müssen wir in unserem Code die Ethernet-Bibliothek importieren:

```Arduino
#include <Ethernet.h>
```

Als nächstes definieren wir unsere Variablen für die IP-Adresse und den Host der Webseite, die wir herunterladen möchten:

```Arduino
byte server[] = { 173,194,112,115 }; 

// Das ist die IP-Adresse von Google. 
// Du kannst sie durch die IP-Adresse oder den Host deiner Zielseite ersetzen.
```

Dann erstellen wir ein Ethernet-Client-Objekt und stellen eine Verbindung zum Server her:

```Arduino
EthernetClient client;
if (client.connect(server, 80)) {
  // Verbindung erfolgreich hergestellt
}
```

Als nächstes senden wir einen HTTP-GET-Request an den Server, um die Seite herunterzuladen und die Antwort in einem String zu speichern:

```Arduino
client.print("GET / HTTP/1.1\n");
client.print("Host: google.com\n");
client.print("Connection: close\n");
client.println();
String response = client.readStringUntil('\n');
```

Wir schließen die Verbindung und können nun den heruntergeladenen Inhalt verarbeiten, beispielsweise indem wir ihn auf dem seriellen Monitor ausgeben:

```Arduino
client.stop();
Serial.println(response);
```

## Tiefere Einblicke

Wir haben nun gesehen, wie einfach es ist, eine Webseite herunterzuladen und ihre Inhalte in unseren Arduino-Code zu integrieren. Aber wie funktioniert das genau? Nun, wenn wir eine Webseite besuchen, fordern wir sie eigentlich über das HTTP-Protokoll an. Dies ist ein Kommunikationsprotokoll, das es ermöglicht, Hypertext-Dokumente im Internet abzurufen. Der HTTP-GET-Request, den wir in unserem Code verwendet haben, ist also die Anforderung, die wir an den Server senden, um die gewünschte Seite herunterzuladen. Die Serverantwort besteht aus einem HTTP-Statuscode, gefolgt von den eigentlichen Inhalten der Seite.

## Siehe auch

- [Ethernet Library Dokumentation für Arduino](https://www.arduino.cc/en/Reference/Ethernet)
- [HTTP-GET-Request Erklärung](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/GET)