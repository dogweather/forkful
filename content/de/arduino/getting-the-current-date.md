---
title:    "Arduino: Den aktuellen Datumsstand erhalten"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum

Wenn Sie sich schon einmal gefragt haben, wie Sie das aktuelle Datum in Ihren Arduino-Projekten verwenden können, dann sind Sie hier genau richtig! Das Abrufen des aktuellen Datums kann nützlich sein, um Zeitstempel für Ereignisse zu erstellen oder um bestimmte Aktionen basierend auf dem Datum auszuführen. In diesem Blogbeitrag lernen Sie, wie Sie das aktuelle Datum mithilfe von Arduino programmieren können.

## Wie man das aktuelle Datum abruft

Die Verwendung der eingebauten Funktionen von Arduino macht es einfach, das aktuelle Datum abzurufen. Zunächst müssen Sie die "Time" Bibliothek in Ihrem Sketch einbinden. Dies geschieht, indem Sie die folgende Zeile Code oben in Ihren Sketch einfügen:

```Arduino
#include <Time.h>
```

Als nächstes müssen Sie eine Variable vom Typ "tmElements_t" erstellen, um das Datum zu speichern. Hier ist ein Beispiel, wie Sie dies tun können:

```Arduino
tmElements_t currentDate;
```

Dann können Sie die aktuelle Zeit und das Datum mit der Funktion "now()" abrufen und in der zuvor erstellten Variablen speichern:

```Arduino
currentDate = now();
```

Um das Datum im Serienmonitor anzuzeigen, können Sie die entsprechenden Werte aus der Variablen extrahieren und ausgeben. Zum Beispiel:

```Arduino
Serial.print("Aktuelles Datum: ");
Serial.print(currentDate.Day);
Serial.print(".");
Serial.print(currentDate.Month);
Serial.print(".");
Serial.println(currentDate.Year);
```

Die Ausgabe sollte in etwa so aussehen: "Aktuelles Datum: 18.05.2021".

Jetzt können Sie das aktuelle Datum in Ihrem Arduino-Projekt verwenden, um verschiedene Aufgaben und Aktionen zu steuern.

## Tiefergehende Informationen

Die Funktion "now()" gibt die aktuelle Zeit und das Datum als Unix-Zeitstempel zurück. Dies ist die Anzahl der Sekunden seit dem 1. Januar 1970 um 00:00 Uhr UTC. Dieser Wert kann in verschiedene Zeitformate umgewandelt werden, um das Datum in verschiedenen Formaten anzuzeigen.

Es gibt auch weitere Funktionen in der "Time" Bibliothek, mit denen Sie das Datum und die Zeit manipulieren können, z.B. "hour()", "minute()", "second()", etc. Diese können nützlich sein, wenn Sie bestimmte Aufgaben basierend auf spezifischen Uhrzeiten ausführen möchten.

## Siehe auch

- Offizielle "Time" Bibliothek für Arduino: https://www.arduino.cc/reference/en/libraries/time/
- Dokumentation zur "tmElements_t" Struktur: https://www.arduino.cc/reference/en/libraries/time/tmelementst/
- Informationen zum Unix-Zeitstempel: https://www.unixtimestamp.com/