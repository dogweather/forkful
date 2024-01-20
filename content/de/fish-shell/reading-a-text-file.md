---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen einer Textdatei ist ein häufiger Vorgang, bei dem Daten aus einer gespeicherten Datei in einer lesbarer Form ausgelesen werden. Als Programmierer führen wir diese Aufgabe aus, um Informationen, die wir für unsere Programme benötigen, abzurufen und zu manipulieren.

## Anleitung:

In Fish Shell können wir den `cat` oder `less` Befehl verwenden, um den Inhalt einer Datei zu lesen. Hier ist ein einfacher Code:

```Fish Shell
cat Dateiname.txt
```

In diesem Beispiel liest `cat` die Datei `Dateiname.txt` und gibt ihren Inhalt auf dem Bildschirm aus.

Ein anderes Beispiel wäre der `less` Befehl:

```Fish Shell
less Dateiname.txt
```

Dieser Befehl liest auch die Datei `Dateiname.txt` und ermöglicht es Ihnen, durch den Text zu blättern.

## Vertiefung:

Historisch gesehen stammt der `cat` Befehl aus den Bell Laboratories und steht für "concatenate". Der `less` Befehl hingegen wurde als verbesserte Version des `more` Befehls erstellt, mit zusätzlichen Funktionen wie der Rückwärtsnavigation.

Alternativ könnten Sie `more`, `head` oder `tail` verwenden, um Teile einer Datei zu lesen. Die Auswahl hängt von Ihrem spezifischen Bedarf ab.

In Fish Shell werden diese Lesemethoden durch den internen Code des Befehlssystems implementiert, das direkte Systemaufrufe verwendet, um Dateioperationen durchzuführen.

## Siehe auch:

Weitere Informationen erhalten Sie in diesen Quellen:
- Fish Shell Dokumentation: https://fishshell.com/docs/current/index.html
- UNIX cat Befehlsdokumentation: http://manpages.ubuntu.com/manpages/cosmic/man1/cat.1.html
- UNIX less Befehlsdokumentation: http://manpages.ubuntu.com/manpages/cosmic/man1/less.1.html