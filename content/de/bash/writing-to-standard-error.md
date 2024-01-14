---
title:    "Bash: Schreiben zu Standardfehler"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Warum

Das Schreiben auf den Standardfehler kann eine nützliche Technik sein, um Fehlermeldungen und Warnungen in der Bash-Programmierung zu verwalten. Wenn ein Fehler oder eine unerwünschte Ausgabe auftritt, wird es auf dem Standardfehlerkanal ausgegeben, anstatt auf dem Standardausgabe- oder Bildschirmkanal. Dies ermöglicht eine bessere Fehlerbehandlung und Diagnose während der Ausführung des Skripts.

## So geht's

Die Syntax zum Schreiben auf den Standardfehler in Bash lautet: `>&2 echo "Fehlermeldung"`

Dieser Befehl gibt die angegebene Fehlermeldung auf dem Standardfehlerkanal aus. Hier ist ein Beispiel, in dem wir versuchen, eine nicht vorhandene Datei zu öffnen und die Fehlermeldung auf den Standardfehlerkanal ausgeben:

```Bash
#!/bin/bash

if [[ ! -f test.txt ]]; then
    >&2 echo "Die Datei test.txt existiert nicht."
fi
```

Die Ausgabe dieses Skripts wäre: `Die Datei test.txt existiert nicht.` auf dem Standardfehlerkanal.

## Tief ergründen

In der Bash-Programmierung gibt es verschiedene Möglichkeiten, Fehlermeldungen und Warnungen zu verwalten. Das Schreiben auf den Standardfehlerkanal ist eine davon. Es gibt auch Möglichkeiten, den Standardfehlerkanal umzuleiten oder zu unterdrücken, je nach den Anforderungen des Skripts.

Um den Standardfehlerkanal umzuleiten, können wir den Operator `2>` verwenden, gefolgt vom Namen der Datei, auf die die Fehlermeldungen geschrieben werden sollen. Auf diese Weise können wir die Fehlermeldungen in einer separaten Datei speichern, um sie später zu analysieren.

Ein weiterer nützlicher Befehl ist `exec 2>&1`, der den Standardfehlerkanal auf den Standardausgabekanal umleitet. Das bedeutet, dass alle Fehlermeldungen auf demselben Kanal wie die normalen Ausgaben erscheinen und für den Benutzer leichter zu lesen sind.

## Siehe auch
- [Bash-Programmierung](https://wiki.ubuntuusers.de/Bash/Programming/)
- [Grundlagen der Bash-Programmierung](https://www.tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)
- [Standard Input, Output, und Fehlerkanäle](https://www.tldp.org/LDP/abs/html/io-redirection.html)

Vielen Dank fürs Lesen! Wir hoffen, dass dieser Artikel Ihnen dabei geholfen hat, das Schreiben auf den Standardfehlerkanal in Bash besser zu verstehen. Weitere Informationen zur Bash-Programmierung finden Sie in den oben aufgeführten Links. Happy coding!