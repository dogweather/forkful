---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Erstellen einer temporären Datei bezieht sich auf die Praxis, Dateien für kurze Zeiträume während der Ausführung eines Programms zu generieren. Diese Methode ist besonders hilfreich, um Daten zwischen verschiedenen Prozessen auszutauschen, Speicherplatz zu sparen und Anwendungen effizienter zu machen.

## So geht's:

Erstellen einer temporären Datei in Fish Shell ist ein einfacher Prozess. Der Befehl `mktemp` wurde entwickelt, um diese Aufgabe zu erfüllen.

```Fish Shell
# Temporäre Datei anlegen
set temp_file (mktemp)
# Ausgabe der temporären Dateinamen
echo $temp_file
```

Die Ausgabe könnte so aussehen:

```Fish Shell
/tmp/tmp.Iyv9XR1G59
```

Diese Ausgabe zeigt den Namen der gerade erstellten temporären Datei an.

## Vertiefung:

Fish Shell hat seine Wurzeln in der lange existierenden Unix-Shell-Tradition. `mktemp`, das im obigen Beispiel verwendet wurde, ist seit den frühen Unix-Tagen mit uns.

Beachten Sie, es gibt alternative Methoden zur Erstellung temporärer Dateien. Sie könnten beispielsweise `touch /tmp/$RANDOM`, verwenden.

Die Implementationsdetails von `mktemp` können je nach Betriebssystem variieren. In der Regel erstellt `mktemp` eine eindeutige Datei, und Sie können sogar eine Template-Zeichenfolge angeben, um den Namen der temporären Datei zu kontrollieren.

## Siehe auch:

Hier sind einige nützliche Links für weitere Informationen:

- Fish Shell Dokumentation: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- Fish Shell Github Repository: [https://github.com/fish-shell/fish-shell](https://github.com/fish-shell/fish-shell)
- Unix mktemp Man Seite: [https://man7.org/linux/man-pages/man3/mktemp.3.html](https://man7.org/linux/man-pages/man3/mktemp.3.html)