---
title:                "HTML-Parsing."
html_title:           "Arduino: HTML-Parsing."
simple_title:         "HTML-Parsing."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
Parsing HTML ist der Prozess des Lesens und Interpretierens von HTML-Code, um seine Struktur und Inhalte zu verstehen. Programmierer nutzen dies häufig, um Websites zu analysieren oder Daten daraus zu extrahieren.

## Wie Geht's?
Um HTML mit Arduino zu parsen, gibt es verschiedene Bibliotheken wie beispielsweise "ArduinoJson" oder "HtmlParser". Hier ist ein Beispielcode, der die Website "www.google.com" liest und die Überschrift ausgibt:
```
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600); //initialisiere serielle Kommunikation

  StaticJsonDocument<200> doc; //erstelle ein leeres JsonDocument mit 200 Bytes Speicherplatz
  String html = ""; //erstelle eine leere String-Variable

  WiFi.begin("SSID", "Password"); //verbinde mit dem WLAN-Netzwerk
  while (WiFi.status() != WL_CONNECTED) { //warte auf Verbindung
    delay(500);
    Serial.println("Connecting to WiFi..");
  }

  HTTPClient http; //erstelle ein HTTP-Objekt
  http.begin("http://www.google.com"); //starte die Verbindung mit der Website
  int httpCode = http.GET(); //führe ein GET-Request aus
  if (httpCode > 0) { //wenn Verbindung erfolgreich
    html = http.getString(); //lese den HTML-Code in die String-Variable
  }
  else { //sonst
    html = ""; //setze die String-Variable auf leer
  }
  
  deserializeJson(doc, html); //parsen des HTML-Codes
  String headline = doc["head"]["title"]; //extrahieren der Überschrift
  Serial.println(headline); //Ausgabe der Überschrift
}

void loop() {
  //nichts hier
}
```

Die Ausgabe des Beispiels wird in der seriellen Monitor angezeigt und sollte in etwa so aussehen:
```
Google
```

## Tief Tauchen
Parsing HTML wurde ursprünglich entwickelt, um das World Wide Web zu ermöglichen. Heutzutage gibt es jedoch viele Alternativen wie JSON oder XML, die jeweils ihre eigenen Vor- und Nachteile haben. Bei der Implementierung von HTML-Parsing ist es wichtig, auf die Performance zu achten, da HTML oft sehr umfangreich sein kann.

## Siehe Auch
- [ArduinoJson Bibliothek](https://github.com/bblanchon/ArduinoJson)
- [HtmlParser Bibliothek](https://github.com/johnnyb/HtmlParser)