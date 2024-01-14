---
title:    "Bash: Ein neues Projekt beginnen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich dafür entscheiden, ein neues Projekt zu starten? Es gibt viele Gründe, warum man sich als Programmierer oder Programmiererin für ein neues Projekt entscheiden könnte. Vielleicht möchtest du etwas Neues ausprobieren, dein Wissen vertiefen oder einfach eine Lösung für ein bestimmtes Problem finden. Egal aus welchem Grund, es ist immer aufregend, ein neues Projekt zu starten!

## Wie Geht Es

Aber wie fängt man eigentlich an? Zunächst einmal solltest du dir Gedanken darüber machen, welche Programmiersprache du verwenden möchtest. Wir werden hier Beispiele mit Bash zeigen, einer Skriptsprache für die Arbeit in der Terminal-Umgebung. Mit Bash kannst du Befehle und Programme schnell und effektiv ausführen.

Um ein neues Projekt in Bash zu beginnen, kannst du einfach einen neuen Ordner mit dem Namen deines Projekts erstellen. Nennen wir ihn zum Beispiel "Mein Projekt". Öffne dann deinen Terminal und navigiere in den Ordner "Mein Projekt". Nun kannst du mit dem Befehl `touch` eine neue Datei erstellen, die wir "script.sh" nennen werden. Diese Datei wird unser Skript enthalten, mit dem wir unser Projekt ausführen können.

```Bash
mkdir Mein-Projekt
cd Mein-Projekt
touch script.sh
```

Jetzt können wir beginnen, unser Skript zu schreiben. Wir werden mit dem Befehl `echo` eine Ausgabe erstellen und sie dann auf dem Bildschirm anzeigen lassen. Füge dazu folgenden Code in deine "script.sh" Datei ein:

```Bash
#!/bin/bash
echo "Willkommen zu meinem Projekt!"
```

Damit unser Skript auch ausgeführt werden kann, müssen wir es noch ausführbar machen. Das können wir mit dem Befehl `chmod` tun, gefolgt von `+x` und dem Namen unserer Datei, in diesem Fall "script.sh".

```Bash
chmod +x script.sh
```

Nun kannst du dein Skript ausführen, indem du einfach den Namen deiner Datei in deinem Terminal eingibst.

```Bash
./script.sh
```

Und schon wird die Ausgabe "Willkommen zu meinem Projekt!" auf deinem Bildschirm angezeigt.

## Tiefer Eintauchen

Natürlich gibt es noch viel mehr, was du mit Bash machen kannst, um dein Projekt zu verbessern und zu erweitern. Expressions, Variablen und Schleifen sind nur einige der Dinge, die dir dabei helfen können, komplexe Skripte zu erstellen. Indem du dich tiefer in die Welt der Bash-Programmierung hineinbegibst, wirst du in der Lage sein, leistungsstarke und effiziente Lösungen für deine Projekte zu entwickeln.

## Siehe auch

- [Bash-Programmierung für Einsteiger und Fortgeschrittene](https://www.linux-community.de/ausgaben/linuxuser/2010/08/bash-programmierung/)
- [Bash-Handbuch für Anfänger](https://wiki.ubuntuusers.de/Bash/Programmierung/Einfuehrung/)
- [Bash-Skripte auf Bash-Hackers Wiki](https://wiki.bash-hackers.org/)