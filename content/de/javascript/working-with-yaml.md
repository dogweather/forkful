---
title:                "Arbeiten mit yaml"
html_title:           "Javascript: Arbeiten mit yaml"
simple_title:         "Arbeiten mit yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

# Warum

Warum sollte jemand mit YAML arbeiten? Nun, es gibt einige gute Gründe dafür. Erstens ist YAML eine einfache und elegante Möglichkeit, Daten zu strukturieren und zu organisieren. Es ist auch eine sehr flexible Methode, um Konfigurationsdateien zu erstellen, die von verschiedenen Programmen gelesen werden können. Ob Sie also an der Entwicklung von Anwendungen beteiligt sind oder einfach nur Ihre persönlichen Einstellungen organisieren möchten, YAML kann dabei eine große Hilfe sein.

# Wie

Um mit YAML zu arbeiten, benötigen Sie zunächst eine Texteditor-Software. Es gibt viele Optionen da draußen, aber ich empfehle VSCode oder Atom, da sie speziell für die Entwicklung von JavaScript optimiert sind.

Sobald Sie Ihren Texteditor geöffnet haben, erstellen Sie eine neue Datei mit der Dateiendung ".yml" oder ".yaml". Dies ist wichtig, damit das Programm erkennen kann, dass es sich um eine YAML-Datei handelt.

Nun können Sie anfangen, Ihren Code zu schreiben. YAML verwendet Einrückungen und Leerzeichen, um Daten hierarchisch zu strukturieren. Zum Beispiel:

```javascript
hobby:
  - name: Fotografie
    jahr: 2018
  - name: Lesen
    jahr: 2020
  - name: Wandern
    jahr: 2021
```

Dieser Code erstellt eine Liste von Hobbys mit dem Namen und dem Jahr, in dem sie begonnen wurden. Beachten Sie die Einrückungen vor den "-"-Zeichen, um die Elemente der Liste voneinander zu trennen.

Sie können auch Variablen und Werte definieren, ähnlich wie bei anderen Programmiersprachen. Zum Beispiel:

```javascript
name: Max
alter: 25
beruf: Entwickler
```

Wenn Sie das obige Beispiel in eine YAML-Datei schreiben und speichern, können Sie sie dann von einer anderen Anwendung auslesen und die Variablen verwenden.

# Tiefgehende Informationen

YAML bietet auch eine Vielzahl von Funktionen wie Kommentare, Vererbung und Inline-Verweise. Es ist wichtig, sich mit diesen Funktionen vertraut zu machen, um effektiv mit YAML zu arbeiten.

Eines der nützlichsten Features von YAML ist die Fähigkeit, Konfigurationsdateien zu erstellen. Viele Anwendungen, einschließlich JavaScript-Programme, verwenden YAML-Dateien, um verschiedene Einstellungen zu speichern. Dies erleichtert die Anpassung und Wartung der Anwendung.

Ein weiterer Vorteil von YAML ist, dass es plattform- und programmiersprachenunabhängig ist. Dies bedeutet, dass Sie es für verschiedene Anwendungen und in verschiedenen Umgebungen verwenden können, ohne sich um Kompatibilitätsprobleme sorgen zu müssen.

# Siehe auch

- [YAML Offizielle Website](https://yaml.org/)
- [VSCode](https://code.visualstudio.com/)
- [Atom](https://atom.io/)