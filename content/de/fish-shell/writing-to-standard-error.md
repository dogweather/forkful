---
title:                "Fish Shell: Schreiben nach Standardfehler"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Fehlern in die Standardfehlerausgabe ist eine nützliche Technik, um Probleme und Fehler in Ihren Fish-Shell-Skripten zu identifizieren und zu beheben. Mit dieser Methode können Sie genauere Informationen über die Fehlerursache erhalten und Ihre Skripte effizienter debuggen.

## Wie das geht

Das Schreiben in die Standardfehlerausgabe ist in der Fish-Shell sehr einfach. Verwenden Sie einfach den Befehl `echo` gefolgt von dem gewünschten Text. Zum Beispiel:

```Fish Shell
echo "Das ist ein Beispieltext für die Standardfehlerausgabe"
```

Dieser Befehl schreibt den Text "Das ist ein Beispieltext für die Standardfehlerausgabe" in die Standardfehlerausgabe. Sie können auch Variablen verwenden, um dynamische Fehlermeldungen zu erstellen.

Eine weitere nützliche Methode ist die Verwendung von Pipes, um den Output von Befehlen in die Standardfehlerausgabe zu leiten. Hier ist ein Beispiel, das den Output des Befehls `ls` in die Standardfehlerausgabe schreibt:

```Fish Shell
ls -l |& echo
```

Sie können auch den Befehl `printf` verwenden, um formatierten Text in die Standardfehlerausgabe zu schreiben. Hier ist ein Beispiel, das den Wert einer Variablen in einer Fehlermeldung ausgibt:

```Fish Shell
set options "Fish Shell"
printf "Die Option %s ist nicht verfügbar" $options |& echo
```

Die Verwendung dieser Techniken kann Ihnen helfen, schnell und effizient Fehler in Ihren Fish-Shell-Skripten zu identifizieren und zu beheben.

## Tiefer Einblick

Das Schreiben in die Standardfehlerausgabe ist nicht nur nützlich beim Debuggen von Skripten, sondern auch beim Erstellen von informativen Benachrichtigungen für den Benutzer. Sie können auch spezielle Fehlercodes verwenden, um bestimmte Arten von Fehlern zu identifizieren und entsprechend zu handeln.

Eine gute Praxis ist es auch, sowohl die Standardausgabe als auch die Standardfehlerausgabe in separate Dateien zu leiten, um eine bessere Übersicht über den Output zu erhalten. Dies kann mit Hilfe von Pipes und dem Befehl `tee` erreicht werden.

## Siehe auch

- [Fish-Shell-Dokumentation über das Schreiben in die Standardfehlerausgabe](https://fishshell.com/docs/current/tutorial.html#tutorial-output)
- [Eine Einführung in die Fish-Shell](https://platzi.com/blog/einfuhrung-in-die-fish-shell/)
- [Offizielle Fish-Shell-Website](https://fishshell.com/)