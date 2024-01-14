---
title:    "Bash: Prüfen, ob ein Verzeichnis vorhanden ist"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Bevor wir uns in die Details vertiefen, fragst du dich vielleicht, warum es überhaupt wichtig ist, zu überprüfen, ob ein Verzeichnis existiert. Nun, es kann verschiedene Gründe geben, warum du diese Überprüfung in deinem Bash-Skript benötigst. Zum Beispiel könnte dein Skript von einem bestimmten Verzeichnis abhängen, um Dateien zu finden oder zu lesen. Wenn das Verzeichnis nicht existiert, kann dein Skript nicht wie erwartet funktionieren. Aus diesem Grund ist es wichtig zu wissen, wie man überprüft, ob ein Verzeichnis vorhanden ist.

## Wie geht's

Es gibt einige Methoden in Bash, um zu überprüfen, ob ein Verzeichnis vorhanden ist. Eine einfache Möglichkeit ist die Verwendung des Befehls "test" mit der Bedingung "-d", gefolgt vom Pfad des Verzeichnisses, das überprüft werden soll. Dies könnte zum Beispiel so aussehen:

```Bash
test -d /home/benutzer/dokumente && echo "Das Verzeichnis existiert."
```

Dieser Befehl überprüft, ob das Verzeichnis "/home/benutzer/dokumente" existiert und gibt dann die Nachricht "Das Verzeichnis existiert." aus, falls dies der Fall ist.

Du kannst auch den Befehl "if" verwenden, um eine Aktion auszuführen, wenn das Verzeichnis existiert, oder eine andere Aktion ausführen, wenn dies nicht der Fall ist. Ein Beispiel könnte so aussehen:

```Bash
if [ -d /home/benutzer/dokumente ]
then
  echo "Das Verzeichnis existiert."
else
  mkdir /home/benutzer/dokumente
  echo "Das Verzeichnis wurde erstellt."
fi
```

Dieser Code überprüft auch, ob das Verzeichnis existiert, führt dann aber verschiedene Aktionen aus, je nachdem, ob das Verzeichnis bereits vorhanden ist oder nicht.

## Tiefere Einblicke

Die Überprüfung eines Verzeichnisses in Bash ist eigentlich recht einfach, aber es gibt ein paar Dinge, die du beachten solltest. Zum Beispiel kann es sein, dass du die Rechte haben musst, um auf das Verzeichnis zugreifen zu dürfen, damit die Überprüfung richtig funktioniert. Außerdem kann es hilfreich sein, die Ausgabe der Überprüfung in einer Variablen zu speichern, um sie später in deinem Skript wiederzuverwenden.

Eine weitere nützliche Methode ist die Verwendung von Wildcards, um nach bestimmten Verzeichnissen zu suchen. Dabei kannst du beispielsweise eine Schleife verwenden, um alle existierenden Verzeichnisse in einem übergeordneten Ordner aufzulisten.

## Siehe auch

- Hier findest du weitere Informationen zu Variablen in Bash: https://www.tutorialspoint.com/unix/unix-shell-variables.htm
- Erfahre mehr über die Verwendung von Wildcards in Bash: https://www.tecmint.com/use-wildcards-for-matching-filenames-in-linux/
- Eine umfangreiche Einführung in die Verwendung von Bedingungen in Bash: https://www.linux.com/topic/desktop/if-statement-bash/