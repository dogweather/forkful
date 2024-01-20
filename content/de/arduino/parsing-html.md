---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

HTML-Parsing ist der Prozess, durch den ein Programm Informationen aus HTML-Daten extrahiert. Programmierer tun dies, um Daten aus Webseiten zu sammeln, die dann in ihren eigenen Anwendungen verwendet werden können.

## So geht's:

Mit Arduino und der Ethernet-Bibliothek ist HTML-Parsing einfach. Nehmen wir an, wir möchten die Überschrift (H1) einer Webseite extrahieren:

```Arduino
#include <Ethernet.h>

void setup() {
  Ethernet.begin(...);
  Serial.begin(9600);
}

void loop() {
  EthernetClient client = Ethernet.connect("beispielwebseite.de", 80);

  while (client.connected()) {
    if (client.available()) {
      char c = client.read();

      if (c == '<') {
        String tag = "";
        while (c != '>') {
          c = client.read();
          tag += c;
        }
        if (tag.startsWith("h1")) {
          String title = "";
          while (!tag.endsWith("/h1")) {
            c = client.read();
            title += c;
          }
          Serial.println(title);
        }
      }
    }
  }
  delay(5000);
}
```

Der obige Code extrahiert alle Überschriften (H1) von der Webseite `beispielwebseite.de` und druckt sie auf der Konsole aus.

## Tiefgang:

1. Historischer Kontext: Der Begriff "parsing" stammt aus dem lateinischen "pars" für "Teil" und wurde im Kontext von grammatikalischer Analyse verwendet, bevor er in der Informatik Anklang fand.

2. Alternativen: Andere Bibliotheken wie BeautifulSoup in Python oder Jsoup in Java, erlauben es Ihnen, HTML zu parsen. Sie sind jedoch komplexer und benötigen mehr Rechenleistung, was in einem Arduino-System selten zur Verfügung steht.

3. Implementierungsdetails: Das obige Beispiel ist sehr rudimentär und nicht gegen Fehler robust. In der Praxis ist es ratsam, eine eigene HTML-Parsing-Bibliothek zu erstellen oder eine fertige, geeignete Bibliothek zu verwenden.

## Siehe auch:

1. Arduino Ethernet-Bibliothek: [link](https://www.arduino.cc/en/Reference/Ethernet)
2. Weitere Information zum Parsing: [link](https://en.wikipedia.org/wiki/Parsing)
3. BeautifulSoup in Python: [link](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
4. Jsoup in Java: [link](https://jsoup.org/)