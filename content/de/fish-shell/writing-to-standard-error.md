---
title:    "Fish Shell: Schreiben auf Standardfehler"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben in die Standardfehlerausgabe (standard error) kann nützlich sein, um Fehlermeldungen und Warnungen in der Kommandozeile sichtbar zu machen. Dadurch können Entwickler Fehler schneller erkennen und beheben, was die Gesamteffizienz des Codings verbessert.

## Wie geht man vor

Um in die Standardfehlerausgabe zu schreiben, kann man das Schlüsselwort "echo" in Kombination mit dem "1>&2" Befehl verwenden. Hier ist ein Beispiel:

```Fish Shell
echo "Fehler: Variable nicht definiert" 1>&2
```

Das Ergebnis wäre dann, dass der Text "Fehler: Variable nicht definiert" in der Standardfehlerausgabe erscheint. Zusätzlich kann man auch den ">&" Befehl nutzen, um den Inhalt einer Datei in die Standardfehlerausgabe zu schreiben.

```Fish Shell
cat datei.txt >&2
```

Dieser Befehl würde den Inhalt der Datei "datei.txt" in die Standardfehlerausgabe schreiben.

## Tiefergehende Informationen

Beim Schreiben in die Standardfehlerausgabe ist es wichtig zu beachten, dass es sich um eine unformatierte Ausgabe handelt. Das bedeutet, dass die Ausgabe nicht manipuliert werden kann und bestehende Formatierungen möglicherweise verloren gehen. Außerdem sollte man darauf achten, dass doppelte Anführungszeichen im Text vermieden werden, da diese zu Problemen führen können.

## Siehe auch

- [Fish Shell Dokumentation über Standardfehlerausgabe](https://fishshell.com/docs/current/cmds/echo.html)
- [Tutorial: Einführung in Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell Cheatsheet](https://fishshell.com/docs/current/tutorial.html#cheatsheet)