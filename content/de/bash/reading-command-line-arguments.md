---
title:                "Lesen von Eingabeargumenten in der Befehlszeile"
html_title:           "Bash: Lesen von Eingabeargumenten in der Befehlszeile"
simple_title:         "Lesen von Eingabeargumenten in der Befehlszeile"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

### Was & Warum?
Das Lesen von Befehlszeilenargumenten ist ein wichtiger Bestandteil der Bash-Programmierung. Es ermöglicht dem Programm, Daten oder Parameter vom Benutzer bei der Ausführung zu erhalten. Programmierer nutzen dies, um ihre Skripte anzupassen und verschiedene Aktionen basierend auf den gegebenen Argumenten auszuführen.

### Wie geht's:
Das Lesen von Befehlszeilenargumenten in Bash ist einfach. Die Argumente können in Variablen gespeichert werden und dann im Skript verwendet werden. Hier ist ein Beispiel, wie man die Argumente "Hallo" und "Welt" in Variablen speichert und ausgibt:

```Bash
argument1=$1
argument2=$2
echo $argument1 $argument2
```

Der Output des Skripts wäre dann: "Hallo Welt". Man kann auch auf einzelne Argumente durch ihre Position zugreifen, z.B. $1 für das erste Argument oder $2 für das zweite Argument und so weiter.

### Tieferer Einblick:
Das Konzept des Lesens von Befehlszeilenargumenten stammt aus dem ursprünglichen UNIX-Betriebssystem, auf dem Bash basiert. Es gibt jedoch auch Alternativen wie das Parsen von Umgebungsvariablen oder das Lesen von Eingaben mit dem "read" Befehl. Diese Methoden können nützlich sein, aber das Lesen von Befehlszeilenargumenten ist in der Regel die effizienteste und bevorzugte Methode.

### Siehe auch:
- [Offizielle GNU Bash Dokumentation](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Guide für Anfänger](https://opensourceforu.com/2017/02/bash-guide-for-beginners/)
- [Bash Befehle Referenz](https://linuxize.com/post/bash-scripting-commands/)