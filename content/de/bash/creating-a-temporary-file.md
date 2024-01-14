---
title:    "Bash: Erstellen einer temporären Datei"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum
Das Erstellen von temporären Dateien ist ein häufig verwendeter Vorgang in der Bash-Programmierung. Es ermöglicht Programmierern, schnell und effizient temporäre Dateien oder Verzeichnisse zu erstellen, die für spezifische Aufgaben oder Prozesse benötigt werden. Das Verwenden temporärer Dateien hilft auch dabei, die Systemressourcen effektiv zu verwalten und vor unerwünschten Speicherlecks zu schützen.

# How To
Das Erstellen einer temporären Datei in Bash ist relativ einfach und erfordert nur wenige Zeilen Code. Hier ist ein Beispiel, in dem wir eine temporäre Datei erstellen und einen Text darin speichern:

```Bash
#!/bin/bash

# Erstellen einer temporären Datei mit dem Namen "temp"
temp=$(mktemp)

# Schreibgeschützte Datei zum Speichern des Textes
readonly text="Hallo, dies ist ein Beispieltext."

# Schreibt den Text in die temporäre Datei
echo "$text" > "$temp"

# Zeigt den Inhalt der temporären Datei an
cat "$temp"
```

Die Ausgabe für diesen Code wird folgendermaßen aussehen:

```
Hallo, dies ist ein Beispieltext.
```

In diesem Beispiel verwenden wir die Befehle "mktemp" und "echo", um die temporäre Datei zu erstellen und Text darin zu speichern. Es ist wichtig, dass wir beim Erstellen einer temporären Datei auch die richtigen Berechtigungen setzen, um sicherzustellen, dass sie nur vom aktuellen Benutzer gelesen, geschrieben oder ausgeführt werden kann. Durch das Verwenden von "readonly" stellen wir sicher, dass die Datei nur im Lesezugriff ist und nicht versehentlich überschrieben werden kann.

# Deep Dive
Der Befehl "mktemp" dient dazu, eine temporäre Datei oder ein Verzeichnis zu erstellen und dabei die vordefinierten Mustern wie "XXX" oder "XXXXXX" zu verwenden. Diese Muster werden dann durch eine zufällige Zeichenfolge ersetzt, um eine eindeutige Datei oder ein Verzeichnis zu erstellen. Durch das Hinzufügen eines Präfixes oder Suffixes in den Mustern können Sie auch den Namen der temporären Datei anpassen.

Es ist auch wichtig zu beachten, dass beim Erstellen einer temporären Datei oder eines Verzeichnisses Speicherplatz auf der Festplatte verwendet wird. Dies ist besonders kritisch bei der Verwendung von Skripten, die häufig temporäre Dateien erstellen, da dies möglicherweise zu einem Engpass beim verfügbaren Speicherplatz führen kann. Daher ist es wichtig, sicherzustellen, dass alle temporären Dateien am Ende des Skripts gelöscht werden, um Ressourcen freizugeben.

# Siehe auch
- [Manpage für mktemp](https://www.man7.org/linux/man-pages/man1/mktemp.1.html)
- [Bash-Skript-Tutorial](https://www.lifewire.com/write-simple-bash-shell-script-2200573)
- [Speicherleck in Skripten vermeiden](https://www.linux.com/topic/desktop/linux-finds-and-fixes-a-major-bash-shell-security-hole/)