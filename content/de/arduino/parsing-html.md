---
title:                "HTML analysieren"
html_title:           "Arduino: HTML analysieren"
simple_title:         "HTML analysieren"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Haben Sie sich jemals gefragt, wie Sie Daten von einer Website auf Ihrem Arduino erhalten können? Das Parsen von HTML ermöglicht es Ihnen, Daten aus dem Internet zu sammeln und sie auf Ihrem Arduino zu verwenden. Dies kann hilfreich sein, um Informationen von verschiedenen Quellen zu sammeln oder sogar um das Verhalten Ihres Arduino basierend auf Daten aus dem Web zu steuern.

## Anleitung

Um HTML auf Ihrem Arduino zu parsen, benötigen Sie die Bibliothek "HTMLParser". Diese kann über die Arduino IDE installiert werden. Stellen Sie sicher, dass Sie die neueste Version verwenden, um mögliche Fehler zu vermeiden. 

Als nächstes importieren Sie die Bibliothek in Ihrem Sketch mit dem Befehl ```Arduino void setup```. Vergessen Sie nicht, auch die Bibliothek mit einem ```Arduino #include``` Befehl anzugeben.

Erstellen Sie nun eine Instanz der Klasse ```Arduino HTMLParser htmlParser```. Diese Instanz wird zum Parseen des HTML-Codes verwendet. 

Danach müssen Sie eine Funktion erstellen, um die Daten aus dem HTML-Code zu extrahieren. Verwenden Sie dafür den Befehl ```Arduino void handleTag```. In dieser Funktion können Sie dann bestimmte Tags auswählen und die darin enthaltenen Daten speichern.

Schließlich müssen Sie nur noch die Funktion ```Arduino void parse``` aufrufen, die den HTML-Code über ein String-Objekt an die ```Arduino handleTag``` Funktion sendet. 

Hier ist ein Beispielcode, der die Temperatur von einer Wetterwebsite extrahiert und auf dem Seriellen Monitor ausgibt:

```
#include <HTMLParser.h>

HTMLParser htmlParser;

void setup() {
  Serial.begin(9600);
  htmlParser = HTMLParser();
}

void handleTag(const char *tag, const char *data) {
  if (strcmp(tag, "span") == 0) {
    Serial.println(data);
  }
}

void parse() {
  String html = "<span id=\"temp\">25°C</span>";
  htmlParser.parse(html.c_str(), html.length(), &handleTag);
}

void loop() {
  parse();
}
```

Daraufhin sollte auf dem Seriellen Monitor "25°C" angezeigt werden.

## Tieferer Einblick

Das Parsen von HTML auf einem Mikrocontroller wie dem Arduino kann eine Herausforderung sein, da es begrenzte Ressourcen und Rechenleistung hat. Deshalb ist es wichtig, dass Sie den HTML-Code auf das Wesentliche reduzieren, um die Effizienz zu maximieren. Vermeiden Sie die Verwendung von CSS oder JavaScript, da dies den Code unnötig verlängern kann.

Sie können auch verschiedene Bibliotheken ausprobieren, um zu sehen, welche am besten für Ihre spezifischen Anforderungen geeignet ist. Einige mögliche Alternativen sind "EZHTML", "htmlcxx" und "IPWorks PNG HTML5 Parser".

## Siehe auch

Weitere Informationen zum Parsen von HTML auf einem Arduino finden Sie in den folgenden Ressourcen:

- [HTMLParser Bibliothek Dokumentation](https://playground.arduino.cc/Code/HTMLParser)
- [Tutorial: Parsing HTML mit Arduino und der HTMLParser Bibliothek](https://www.tweaking4all.com/hardware/arduino/arduino-parsing-simple-html-with-the-htmlparser-library/)
- [Videoanleitung: Daten aus einer Website mit dem Arduino parsen](https://www.youtube.com/watch?v=4vEGE7TYEC0)