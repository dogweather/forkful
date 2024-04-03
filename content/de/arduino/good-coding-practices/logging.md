---
date: 2024-01-26 00:58:30.878939-07:00
description: "\"Logging\" bezeichnet das Aufzeichnen von Ereignissen, Transaktionen\
  \ oder Aktivit\xE4ten, die im Laufe der Zeit in einem System stattfinden. Programmierer\u2026"
lastmod: '2024-03-13T22:44:54.149986-06:00'
model: gpt-4-1106-preview
summary: "\"Logging\" bezeichnet das Aufzeichnen von Ereignissen, Transaktionen oder\
  \ Aktivit\xE4ten, die im Laufe der Zeit in einem System stattfinden."
title: Protokollierung
weight: 17
---

## Wie geht das:
Arduino verfügt nicht über eine eingebaute Logging-Bibliothek wie einige andere Umgebungen, aber man kann ein einfaches Logging auf der seriellen Konsole mit minimalem Aufwand implementieren. Hier ein schnelles Beispiel, um Ihnen den Einstieg zu erleichtern:

```arduino
void setup() {
  // Beginne die serielle Kommunikation mit der angegebenen Baudrate
  Serial.begin(9600);

  // Warte, bis die serielle Schnittstelle verbunden ist – nur auf manchen Boards nötig
  while (!Serial) {
    ; // Warte auf Verbindung der seriellen Schnittstelle. Notwendig für native USB-Anschlüsse
  }

  // Protokolliere eine Informationsmeldung, die anzeigt, dass der Aufbauvorgang abgeschlossen ist
  Serial.println("Setup abgeschlossen!");
}

void loop() {
  // Einfacher Logger, der die Betriebszeit jede Sekunde ausgibt
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Betriebszeit (ms): ");
    Serial.println(currentMillis);

    // Hier könnten Sie auch Fehlerprotokolle, Warnungen oder andere Informationen hinzufügen.
  }
  
  // Rest Ihrer Programlogik hier...
}
```

Beispielausgabe der seriellen Schnittstelle:
```
Setup abgeschlossen!
Betriebszeit (ms): 1000
Betriebszeit (ms): 2000
Betriebszeit (ms): 3000
...
```

## Tiefergehend:
Historisch gesehen war Logging auf Mikrocontrollern nicht so einfach wie auf einem vollwertigen Betriebssystem. Begrenzte Ressourcen bedeuteten, dass jeder Byte zählte und Entwickler mussten darauf achten, das System nicht zu verstopfen. Mit dem Aufkommen leistungsfähigerer Platinen und der Vereinfachung des Prozesses durch die Arduino-Plattform ist das Logging zugänglicher geworden.

Während der obenstehende Code das Logging über die serielle Schnittstelle demonstriert, beinhalten andere Methoden das Schreiben auf eine SD-Karte, das Senden von Daten über ein Netzwerk an einen entfernten Server oder sogar die Ausgabe auf einem kleinen LCD.

Die Implementierung eines Logging-Systems bringt Überlegungen wie Rotation, Schweregrad der Protokollierung (Info, Debug, Warnung, Fehler) und Auswirkungen auf die Leistung mit sich. Auf einem Arduino könnte es notwendig sein, auf Speicherbeschränkungen zu achten, wenn komplizierte Datenstrukturen protokolliert werden. Beim Remote-Logging ist auch die Sicherheit der übertragenen Protokolle eine Überlegung wert.

Mehr ausgefeilte Lösungen wie Syslog, ein weit verbreiteter Logging-Standard, existieren außerhalb der Arduino-Welt, aber man kann Drittanbieter-Bibliotheken integrieren, die ähnliche Funktionalität mit verschiedenen Graden an Komplexität und Ressourcenanforderungen bieten.

## Siehe auch:
- [Arduino `Serial` Referenz](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [SD-Karten-Logging mit Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [SparkFuns Datenlogging-Shield](https://www.sparkfun.com/products/13712)
- [TinyWeb: Ein praktisches Beispiel für Remote-Logging mit Arduino](https://www.arduino.cc/en/Tutorial/WebClientRepeating)
