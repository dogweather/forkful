---
title:                "Bash: Das Lesen von Befehlszeilenargumenten"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung gibt es eine Vielzahl von Werkzeugen und Technologien. Eines davon ist die Bash-Programmierung, die für die Arbeit mit der Bash-Befehlszeile verwendet wird. Eine wichtige Fähigkeit bei der Bash-Programmierung ist das Lesen von Befehlszeilenargumenten. In diesem Blog-Beitrag werden wir uns genau anschauen, warum es wichtig ist, diese Fähigkeit zu beherrschen.

## Wie geht das?

Das Lesen von Befehlszeilenargumenten ist eine Methode, die es uns ermöglicht, Eingaben von Benutzern zu akzeptieren und diese in unserem Code zu verarbeiten. Wir verwenden dafür das `$@` Variable in Bash, die alle Befehlszeilenargumente als einzelne Wörter speichert.

Um dies zu veranschaulichen, betrachten wir ein einfaches Beispiel. Angenommen, wir haben ein Skript mit dem Namen `hello.sh`, das eine persönliche Begrüßung ausgibt. Hier ist der Code dafür:

```Bash
#!/bin/bash
echo "Hallo $1, wie geht es dir?"
```

Um dieses Skript auszuführen, benötigen wir einen Befehlszeilenargument, der unseren Namen enthält. Wir können dies tun, indem wir folgenden Befehl ausführen:

```Bash
./hello.sh "Max"
```

Der Output würde dann wie folgt aussehen:

```Bash
Hallo Max, wie geht es dir?
```

Hier sehen wir, wie das Befehlszeilenargument `Max` an die Variable `$1` übergeben wurde und in unserem Output verwendet wurde.

## Tiefergehende Informationen

Das Lesen von Befehlszeilenargumenten ermöglicht es uns, dynamischere und interaktivere Skripte zu schreiben. Wir können verschiedene Szenarien und Eingaben von Benutzern berücksichtigen und unser Skript entsprechend anpassen.

Es gibt auch verschiedene Möglichkeiten, mit Befehlszeilenargumenten umzugehen, wie zum Beispiel das Überprüfen auf die Anzahl der Argumente oder das Verarbeiten von Optionen. Eine tiefere Auseinandersetzung mit diesen Themen würde den Rahmen dieses Blog-Beitrags sprengen, aber es ist definitiv ein wichtiger Aspekt der Bash-Programmierung, der es wert ist, weiter erforscht zu werden.

## Siehe auch

- [Bash-Argumentverarbeitung](https://www.tldp.org/LDP/abs/html/internalvariables.html#ARGLIST)
- [Bash-Parametererweiterungen](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)

Schlussfolgerung

Das Lesen von Befehlszeilenargumenten ist eine grundlegende Fähigkeit in der Bash-Programmierung, die es uns ermöglicht, interaktive Skripte zu schreiben und Benutzereingaben zu verarbeiten. Wir haben in diesem Blog-Beitrag einige Beispiele gesehen und einen Einblick in die verschiedenen Möglichkeiten bekommen, wie wir Befehlszeilenargumente nutzen können. Wir hoffen, dass dieser Beitrag dazu beigetragen hat, Ihr Verständnis für dieses wichtige Konzept zu vertiefen.