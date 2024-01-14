---
title:                "Fish Shell: Arbeiten mit Yaml"
simple_title:         "Arbeiten mit Yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

Warum: YAML ist eine einfache und benutzerfreundliche Möglichkeit, Daten in Textform zu speichern und zu organisieren. Es ist besonders hilfreich für die Konfiguration von Programmen und ermöglicht eine einfache Kommunikation zwischen verschiedenen Anwendungen.

Wie geht's: Um YAML in Fish Shell zu verwenden, muss zuerst die Platin-Fish-YAML-Erweiterung installiert werden. Dann kann YAML in der Shell verwendet werden, um Daten zu lesen und zu schreiben. Zum Beispiel:

```Fish Shell
set data (yaml eval (cat example.yaml))
```

Dieser Code liest die Datei "example.yaml" und speichert die darin enthaltenen Daten in der Variablen "data". Um YAML-Daten in eine Datei zu schreiben, kann der Befehl "yaml prune" verwendet werden, wie in diesem Beispiel gezeigt:

```Fish Shell
yaml prune data > output.yaml
```

Dieser Code nimmt die Daten aus der Variablen "data" und schreibt sie in die Datei "output.yaml". Weitere Beispiele und Anwendungsfälle können in der offiziellen Dokumentation der Platin-Fish-YAML-Erweiterung gefunden werden.

Tiefentauchen: YAML bietet auch eine Vielzahl von Funktionen, die die Arbeit mit Daten noch leistungsfähiger machen. Zum Beispiel können Sie mithilfe von YAML-Anker und Referenzen Daten wiederverwenden oder mithilfe von Schleifen durch komplexe Datenstrukturen navigieren.

Eine der nützlichsten Funktionen von YAML ist die Verwendung von Umgebungsvariablen. Mit YAML-Variablen können Sie sensible Daten wie Passwörter sicher speichern, ohne sie direkt in der YAML-Datei angeben zu müssen. Sie können dann diese Variablen in Ihrem Fish-Shell-Skript verwenden, um diese sensiblen Daten zu erhalten.

Weitere Informationen zu den verschiedenen Funktionen von YAML und deren Verwendung finden Sie in der offiziellen Dokumentation der Sprache.

Siehe auch: 

- Offizielle Dokumentation der Platin-Fish-YAML-Erweiterung: http://doc.platin.fish/en/yaml/

- Offizielle YAML-Website: https://yaml.org/

- Eine Einführung in die YAML-Syntax von Mozilla: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/JSON

Insgesamt ist YAML eine leistungsstarke und vielseitige Möglichkeit, Daten in der Fish Shell zu organisieren und zu verarbeiten. Ich hoffe, dieser Artikel hat Ihnen geholfen, ein besseres Verständnis davon zu bekommen, wie Sie diese Sprache in Ihrer täglichen Programmierarbeit nutzen können. Viel Spaß beim Coden mit YAML!