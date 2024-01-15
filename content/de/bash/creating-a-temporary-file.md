---
title:                "Erstellen einer temporären Datei"
html_title:           "Bash: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Das Erstellen von temporären Dateien kann in der Bash-Programmierung nützlich sein, um vorübergehend Daten zu speichern, die während des Ausführungsprozesses benötigt werden, aber nicht dauerhaft auf der Festplatte gespeichert werden müssen.

# Wie geht's

Um eine temporäre Datei in Bash zu erstellen, können Sie den Befehl "mktemp" verwenden, gefolgt von einem Präfix für die Datei. Zum Beispiel:
```Bash 
temp_file=$(mktemp prefix.XXXXXX)
```
Dies erstellt eine temporäre Datei mit dem Muster "prefix.XXXXXX", wobei die Xen als Platzhalter für zufällige Zeichen verwendet werden. Der Befehl gibt den erstellten Dateinamen zurück, der in der Variablen "temp_file" gespeichert wird.

Sie können auch eine bestimmte Anzahl von Zeichen im Präfix angeben, z.B. "prefix.XXXXXX.txt" oder "prefix.XXXXXX.html". Dadurch wird die Erweiterung der temporären Datei festgelegt.

Um die temporäre Datei zu verwenden, können Sie sie in einem Befehl ausführen oder Daten in sie schreiben.
```Bash 
echo "Dies ist ein Beispieltext" > $temp_file
# Führt den Inhalt der temporären Datei aus
$temp_file
# Gib den Inhalt der temporären Datei aus
cat $temp_file
```
Wenn Sie mit der temporären Datei fertig sind, sollten Sie sie löschen, um Speicherplatz freizugeben. Verwenden Sie dazu den Befehl "rm".
```Bash 
rm $temp_file
```

# Tiefes Eintauchen

In Bash können Sie auch eine temporäre Datei auf einer bestimmten Dateisystempartition erstellen, indem Sie den Parameter "-p" verwenden und den Pfad zur gewünschten Partition angeben.

Der Befehl "mktemp" kann auch sowohl für Einzel- als auch für Mehrfachverwendung konfiguriert werden, indem Sie die Option "-u" oder "-t" angeben. Das vorherige Beispiel würde dann folgendermaßen aussehen:
```Bash 
temp_file=$(mktemp -u prefix.XXXXXX)
```
Dies würde eine Datei mit dem gleichen Präfix erstellen, aber mit einer zufälligen Nummer als Erweiterung, die für jede Verwendung eindeutig sein würde.

Eine andere Möglichkeit, eine temporäre Datei zu erstellen, ist die Verwendung von Pipes. Sie können den Befehl "mkfifo" verwenden, um eine Pipe-Datei zu erstellen, die als temporäre Datei verwendet werden kann.

# Siehe auch

- [Bash Scripting Guide](https://www.gnu.org/software/bash/guide/)
- [mktemp-Befehlsreferenz](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- [Erstellen und Verwenden zeitlich begrenzter Dateien und Pfade in Bash](https://www.cyberciti.biz/tips/shell-scripting-tutorial-create-temporary-files-using-mktemp.html)