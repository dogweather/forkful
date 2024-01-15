---
title:                "Arbeiten mit JSON"
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Arduino ist eine beliebte Plattform für die Entwicklung von Hardwareprojekten. Oft müssen wir dabei Datenströme verarbeiten, die im JSON-Format vorliegen. Deshalb ist es wichtig zu verstehen, wie man mit JSON bei Arduino umgeht, um komplexe Anwendungen zu entwickeln.

## Wie Geht's

Die Behandlung von JSON in Arduino besteht aus zwei Schritten: Zunächst muss das JSON-Paket aus dem Datenstrom extrahiert und dann interpretiert werden. Dazu verwenden wir die ArduinoJson Library, die uns sowohl das Parsen von JSON als auch das Erstellen von Inhaltsobjekten ermöglicht.

Um JSON einzubinden, navigieren Sie in der Arduino-IDE zu `Sketch` > `Include Library` > `Manage Libraries`. Geben Sie dann im Suchfeld "ArduinoJson" ein und installieren Sie die Bibliothek.

Als nächstes definieren wir eine Empfangspuffergröße, in der das JSON-Paket übertragen wird, und eine Variable, in der die Daten gespeichert werden:

    ```arduino
    #include <ArduinoJson.h> 
    const int BUFFER_SIZE = 200; 
    char jsonBuffer[BUFFER_SIZE];
    ```

Dann erstellen wir ein `StaticJsonDocument` und lesen den Datenstrom in den Puffer:

    ```arduino
    StaticJsonDocument<BUFFER_SIZE> doc;
    deserializeJson(doc, jsonBuffer);
    ```

Um auf die Daten zuzugreifen, können wir die `JsonObject`-Methode verwenden:

    ```arduino
    JsonObject& data = doc.to<JsonObject>();
    ```

Wir können nun auf die im JSON-Paket enthaltenen Werte über ihren Schlüssel zugreifen. Nehmen wir an, das JSON-Paket enthält den Wert für die Temperatur und wir möchten diesen in eine Variable `temp` speichern:

    ```arduino
    float temp = data["temp"];
    ```

Um ein JSON-Dokument zu erstellen und zu senden, können wir die `JsonDocument`-Klasse verwenden:

    ```arduino
    JsonDocument doc;
    doc["name"] = "Max";
    doc["age"] = 30;
    serializeJson(doc, Serial); 
    // Output:{"name":"Max","age":30}
    ```

## Tiefer Eintauchen

Die Verwendung von JSON in Arduino kann komplexer werden, wenn wir mit verschachtelten Objekten und Arrays arbeiten. In diesem Fall können wir die `JsonArray`-Klasse verwenden, um auf die Elemente zuzugreifen:

    ```arduino
    JsonArray& sensors = doc["sensors"];
    float humidity = sensors[0]["humidity"];
    ```

Auch das Arbeiten mit dynamischen JSON-Dokumenten ist möglich, indem man `DynamicJsonDocument` anstelle von `StaticJsonDocument` verwendet.

Es ist wichtig anzumerken, dass die Größe des JSON-Pakets die Größe des Empfangspuffers nicht überschreiten darf, sonst wird ein Fehler auftreten.

## Siehe auch

- Offizielle ArduinoJson-Dokumentation: https://arduinojson.org/
- Tutorial zu ArduinoJson: https://arduinojson.org/v6/doc/upgrade/