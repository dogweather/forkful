---
title:                "Fish Shell: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man eine Webseite herunterladen? Diese Frage mag sich der ein oder andere stellen, aber es gibt tatsächlich viele Gründe, warum man dies tun würde. Möglicherweise möchten Sie eine Sicherungskopie einer Webseite erstellen, bevor Sie Änderungen daran vornehmen, oder Sie möchten eine bestimmte Seite offline lesen. Oder vielleicht möchten Sie einfach nur lernen, wie man mit dem Fish Shell eine Webseite herunterlädt. Egal aus welchem Grund, in diesem Blogbeitrag werden wir uns damit beschäftigen, wie man mit dem Fish Shell eine Webseite herunterladen kann.

## Wie geht's

Um eine Webseite herunterzuladen, verwenden wir den Befehl `curl`, der standardmäßig auf den meisten Betriebssystemen installiert ist. Um den Download zu starten, müssen Sie zunächst die URL der Webseite kennen, die Sie herunterladen möchten. Ein Beispiel dafür wäre die Webseite "https://example.com".

`fish> curl https://example.com`

Mit diesem einfachen Befehl wird der Inhalt der Webseite heruntergeladen und im Terminal angezeigt. Wenn Sie die Webseite stattdessen in eine Datei speichern möchten, können Sie die Option `-o` verwenden, gefolgt von einem Dateinamen. Zum Beispiel:

`fish> curl -o example.html https://example.com`

Dies wird die Webseite in eine Datei mit dem Namen "example.html" speichern. Sie können auch mehrere Webseiten gleichzeitig herunterladen, indem Sie mehrere URLs als Argumente an den Befehl `curl` übergeben.

Auf diese Weise können Sie nicht nur einfache Webseiten, sondern auch komplexere Seiten mit CSS, JavaScript und anderen Elementen herunterladen. Der Fish Shell ist in der Lage, die Webseite genauso darzustellen, wie sie auch im Browser aussehen würde.

## Deep Dive

Trotz seiner Einfachheit ist der `curl` Befehl sehr vielseitig und bietet viele Optionen, die Sie beim Herunterladen von Webseiten nutzen können. Zum Beispiel können Sie mit der Option `-L` Weiterleitungen von URLs folgen, was nützlich sein kann, wenn Sie auf eine Webseite zugreifen möchten, die auf eine andere Seite umleitet.

Außerdem können Sie mit dem `curl` Befehl auch verschiedene Protokolle verwenden, wie zum Beispiel `ftp` oder `sftp`, um Dateien von einem Server herunterzuladen. Sie können auch verschiedene Authentifizierungsmethoden verwenden, falls die Webseite geschützt ist.

Für fortgeschrittene Nutzer gibt es auch die Möglichkeit, Skripte zu schreiben, die den `curl` Befehl nutzen, um Webseiten automatisch herunterzuladen und zu verarbeiten.

## Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/cmds/curl.html)
- [Offizielle Webseite von curl](https://curl.haxx.se/docs/manual.html)
- [Weitere Informationen zu Webseiten-Downloads mit dem Fish Shell](https://github.com/fish-shell/fish-shell/issues/563)

Jetzt wissen Sie, wie Sie mit dem Fish Shell Webseiten herunterladen können. Viel Spaß beim Ausprobieren und Entdecken der vielfältigen Möglichkeiten, die das Fish Shell bietet. Befassen Sie sich gerne weiter mit dem Thema und probieren Sie verschiedene Optionen aus.