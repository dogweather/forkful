---
title:                "Bash: Überprüfen, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiges Konzept in der Bash-Programmierung. Es ermöglicht uns, Bedingungen zu setzen und verschiedenen Codepfaden zu folgen, je nachdem, ob ein Verzeichnis vorhanden ist oder nicht. In diesem Blog-Beitrag werden wir uns ansehen, warum und wie wir diese Überprüfung durchführen können.

## Wie

Wir können ganz einfach überprüfen, ob ein Verzeichnis existiert, indem wir den Befehl `test -d` verwenden, gefolgt vom Pfad des Verzeichnisses, das wir überprüfen möchten. Hier ist ein Beispiel:

```Bash
test -d /home/username/Documents
```

Dieser Code überprüft, ob das Verzeichnis "Documents" im Home-Verzeichnis des Benutzers "username" existiert. Wenn das Verzeichnis vorhanden ist, gibt es keinen Output. Wenn das Verzeichnis nicht vorhanden ist, erhalten wir eine Fehlermeldung.

Eine andere Möglichkeit, um ein Verzeichnis zu überprüfen, ist die Verwendung des Befehls `if` zusammen mit `test -d`. Hier ist ein Beispiel:

```Bash
if test -d /home/username/Documents; then
  echo "Das Verzeichnis existiert."
else
  echo "Das Verzeichnis existiert nicht."
fi
```

Dieser Code überprüft ebenfalls, ob das Verzeichnis "Documents" vorhanden ist, gibt uns jedoch eine aussagekräftigere Ausgabe, je nachdem, ob das Verzeichnis existiert oder nicht.

## Deep Dive

Die Überprüfung, ob ein Verzeichnis existiert, basiert auf dem Befehl `test`, der verschiedene Tests auf Dateien und Verzeichnisse durchführen kann. `-d` ist einer der Tests, die überprüfen, ob ein Verzeichnis vorhanden ist. Wir können auch andere Tests verwenden, um z.B. zu überprüfen, ob eine Datei existiert (`-f`), ob eine Datei lesbar (`-r`) oder beschreibbar (`-w`) ist usw.

Es ist auch wichtig zu beachten, dass die Überprüfung, ob ein Verzeichnis existiert, nur funktioniert, wenn der Benutzer, der den Befehl ausführt, auch Zugriff auf das Verzeichnis hat. Andernfalls wird die Überprüfung immer als nicht vorhanden zurückgegeben.

## Siehe auch

- [Bash-Testkommando](https://linux.die.net/man/1/test)
- [Überprüfung von Dateien und Verzeichnissen in Bash](https://opensource.com/article/19/7/checking-files-and-directories-bash)