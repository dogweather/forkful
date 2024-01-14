---
title:                "Arduino: Analyse von HTML"
simple_title:         "Analyse von HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/parsing-html.md"
---

{{< edit_this_page >}}

# Warum

Das Einbinden von HTML in Arduino-Projekte ermöglicht es, zusätzliche Funktionen wie das Anzeigen von Text und Bildern auf einem Bildschirm hinzuzufügen. Dies kann hilfreich sein, um Benutzerschnittstellen zu verbessern oder Informationen aus dem Internet in das Projekt einzubinden.

# Wie geht's

Um HTML in Arduino zu parsen, können wir die Bibliothek "HtmlParser" verwenden. Zunächst müssen wir die Bibliothek in unser Projekt einbinden, indem wir den folgenden Codeblock am Anfang unseres Sketches einfügen:

```Arduino
#include <HtmlParser.h> 
```

Dann können wir den HTML-Code als String in unserem Sketch definieren, zum Beispiel:

```
HtmlString html = "<html><body><h1>Hallo Welt!</h1></body></html>";
```

Um den Inhalt des HTML-Codes anzuzeigen, können wir die print() oder println() Funktionen verwenden:

```Arduino
Serial.println(html);
```

Dies gibt den String "Hallo Welt!" in der seriellen Monitor-Ausgabe aus.

# Tiefer Einblick

Die "HtmlParser"-Bibliothek ermöglicht es uns, gezielt nach bestimmten Elementen im HTML-Code zu suchen und den Inhalt auszulesen. Dazu können wir beispielsweise die Funktion `findBetween()` verwenden, um den Inhalt zwischen bestimmten HTML-Tags auszulesen.

Ein Beispiel:

```
HtmlString html = "<html><body><h1>Hallo Welt!</h1><p>Ein Beispieltext.</p></body></html>";
String title = html.findBetween("<h1>","</h1>");
String text = html.findBetween("<p>","</p>");
```

Dieser Code liest den Inhalt zwischen den jeweiligen HTML-Tags aus und speichert ihn in den Variablen `title` und `text`.

Weitere Informationen und Beispiele finden Sie in der offiziellen Dokumentation der "HtmlParser"-Bibliothek.

# Siehe auch

- [Offizielle HtmlParser Dokumentation](https://github.com/Links2004/arduinoWebSockets)
- [Tutorial: HTML parsen mit Arduino](https://www.mikrocontroller-elektronik.de/arduino-tutorial/htm-parser-html-text-zwischen-zwei-tags-auslesen/)
- [Beispielprojekt: Wetterstation mit HTML Integration](https://create.arduino.cc/projecthub/icasdri/how-to-make-a-weather-station-using-arduino-cc1a7a)