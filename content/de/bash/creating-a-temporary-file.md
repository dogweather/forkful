---
title:    "Bash: Erstellen einer temporären Datei"
keywords: ["Bash"]
---

{{< edit_this_page >}}

# Warum

In der Welt der Programmierung tauchen oft Konzepte oder Techniken auf, die auf den ersten Blick überflüssig erscheinen. Aber manchmal gibt es gute Gründe, bestimmte Dinge zu tun. Ein gutes Beispiel dafür ist die Erstellung von temporären Dateien in Bash-Skripten. Obwohl es auf den ersten Blick vielleicht nicht sinnvoll erscheint, eine temporäre Datei zu erstellen, kann dies in vielen Fällen die beste Lösung sein. In diesem Blog-Beitrag werden wir uns damit beschäftigen, warum man eine temporäre Datei in einem Bash-Skript erstellen sollte und wie man es am besten macht.

## Wie geht das

Um eine temporäre Datei in Bash zu erstellen, gibt es verschiedene Möglichkeiten. Die einfachste Methode ist die Verwendung des `mktemp` Befehls, der speziell dafür entwickelt wurde, temporäre Dateien zu erstellen. Hier ist ein einfaches Beispiel:

```Bash
#!/bin/bash
tempfile=$(mktemp)
echo "Dies ist eine Testdatei" > $tempfile
cat $tempfile
```

Der `mktemp` Befehl erstellt eine temporäre Datei mit einem eindeutigen Namen und speichert diesen Namen in der Variablen `tempfile`. Dann wird in die erstellte Datei geschrieben und schließlich mit dem `cat` Befehl der Inhalt der Datei angezeigt.

Eine andere Möglichkeit ist die Verwendung des `trap` Befehls, um die temporäre Datei automatisch zu löschen, sobald das Skript beendet wird:

```Bash
#!/bin/bash
tempfile=$(mktemp)
trap "rm -f $tempfile" EXIT
echo "Dies ist eine Testdatei" > $tempfile
cat $tempfile
```

## Tiefer Einblick

Jetzt fragst du dich vielleicht, warum man überhaupt eine temporäre Datei erstellen sollte, wenn man doch einfach eine normale Datei verwenden kann. Es gibt jedoch einige Vorteile bei der Verwendung von temporären Dateien in Bash-Skripten:

- Sicherheit: Temporäre Dateien werden oft in Situationen verwendet, in denen es wichtig ist, die Privatsphäre oder Integrität von Daten zu schützen. Durch die Verwendung von `mktemp` wird sichergestellt, dass die erstellte Datei eindeutig ist und nur für den Zeitraum des Skripts existiert.

- Flexibilität: Temporäre Dateien können an beliebigen Orten erstellt werden, beispielsweise in einem temporären Ordner oder im aktuellen Arbeitsverzeichnis.

- Performance: Durch die Verwendung von `mktemp` statt des `touch` Befehls wird vermieden, dass mehrere Skripte gleichzeitig versuchen, die gleiche Datei zu erstellen.

In der Regel sollten temporäre Dateien nach der Verwendung gelöscht werden, um Speicherplatz zu sparen. Wenn jedoch aus irgendeinem Grund die Datei nicht gelöscht wird (z.B. bei einem Absturz des Skripts), ist es wichtig, darauf zu achten, dass dies keine Auswirkungen auf andere Skripte oder das System hat.

# Siehe auch

- [Linux Bash Temporary File Usage and Security](https://blog.famzah.net/2010/12/06/temporary-files-in-shell-scripts/)
- [Creating Temporary Files in Bash](https://www.linuxjournal.com/content/creating-temporary-files-bash)