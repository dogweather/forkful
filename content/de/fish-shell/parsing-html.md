---
title:                "HTML parsen"
html_title:           "Fish Shell: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Wenn du jemals eine Webseite analysiert oder Daten von einer externen Quelle extrahiert hast, hast du höchstwahrscheinlich mit HTML zu tun gehabt. Das Parsen von HTML ist eine wichtige Fähigkeit, die dir dabei helfen kann, unstrukturierte Informationen zu sammeln und zu organisieren.

## Wie geht's

Das Parsen von HTML im Fish Shell ist relativ einfach und kann mit ein paar Zeilen Code erreicht werden. Zunächst musst du sicherstellen, dass du das Paket "HTML-XML-utils" installiert hast. Dieses Paket enthält die Befehle "hxnormalize" und "hxselect", die für das Parsen von HTML nützlich sind.

Als nächstes musst du den zu analysierenden HTML-Code in einer Datei speichern. Hier ist ein Beispiel für einen einfachen HTML-Code:

```Fish Shell
<html>
  <body>
    <h1>Hello World!</h1>
    <p>Das ist eine Beispiel Webseite.</p>
  </body>
</html>
```

Mit dem Befehl "hxnormalize" kannst du den HTML-Code in ein XML-Format umwandeln, das einfacher zu analysieren ist:

```Fish Shell
hxnormalize -x <beispiel.html> beispiel.xml
```

Der Befehl "hxselect" ermöglicht es dir, aus dem XML-Code spezifische Elemente auszuwählen und auszugeben. Zum Beispiel, um den Text des Titels auszugeben:

```Fish Shell
hxselect -c "h1" <beispiel.xml>
```

Die Ausgabe wäre: "Hello World!". Du kannst auch spezifische Attribute auswählen, z.B. "href" für Links oder "src" für Bilder.

## Tief tauchen

Das Parsen von HTML im Fish Shell kann viel komplexer werden, wenn du mehrere Seiten auf einmal analysieren möchtest. Du könntest zum Beispiel eine Schleife mit dem Befehl "for" verwenden, um eine Liste von URLs abzurufen und zu parsen. Oder du könntest fortgeschrittene Funktionen wie Regular Expressions verwenden, um noch präzisere Auswahlen zu treffen.

Zusätzlich zu den Befehlen "hxnormalize" und "hxselect" gibt es viele weitere Werkzeuge und Bibliotheken, die dir beim Parsen von HTML im Fish Shell helfen können, wie z.B. "pup" oder "lid".

## Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [HTML-XML-utils](https://www.w3.org/Tools/HTML-XML-utils/)
- [Pup](https://github.com/ericchiang/pup)
- [Lid](https://github.com/udhos/jawl/blob/master/man/README.run-lid.md)