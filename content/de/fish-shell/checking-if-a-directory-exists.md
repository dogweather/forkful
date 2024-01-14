---
title:    "Fish Shell: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum?

Das Überprüfen, ob ein bestimmtes Verzeichnis existiert, ist ein wichtiger Schritt in der Entwicklung von Skripten oder Programmen. Es kann helfen, Fehler und unnötige Arbeit zu vermeiden, indem es sicherstellt, dass bestimmte Bedingungen erfüllt sind, bevor der Code ausgeführt wird.

## Wie man es macht

Um in Fish Shell zu überprüfen, ob ein Verzeichnis existiert, müssen wir den Befehl `test` verwenden, der verschiedene Tests auf Dateien und Verzeichnisse ausführen kann.

```Fish Shell
if test -d "Pfad/zum/Verzeichnis"
    echo "Das Verzeichnis existiert!"
else
    echo "Das Verzeichnis existiert nicht!"
end
```

Beim Ausführen dieses Codes wird entweder "Das Verzeichnis existiert!" oder "Das Verzeichnis existiert nicht!" ausgegeben, je nachdem, ob das Verzeichnis vorhanden ist oder nicht.

## Tiefer Einblick

Der `test`-Befehl wird unter der Haube von Fish Shell durch den Befehl `test` des Betriebssystems ausgeführt, der die Dateiattribute überprüft und je nach Ergebnis eine Wahrheitswert zurückgibt. Mit der Option `-d` überprüfen wir speziell, ob es sich bei dem angegebenen Pfad um ein Verzeichnis handelt.

Es ist auch möglich, mehrere Bedingungen in einer Zeile zu überprüfen, indem `&&` für "und" und `||` für "oder" verwendet werden. Zum Beispiel:

```Fish Shell
if test -d "Pfad/zum/Verzeichnis" && test -e "Pfad/zum/Datei"
    echo "Beide existieren!"
else
    echo "Eines existiert nicht!"
end
```

In diesem Beispiel wird überprüft, ob sowohl das angegebene Verzeichnis als auch die angegebene Datei existieren. Nur wenn beide Bedingungen erfüllt sind, wird "Beide existieren!" ausgegeben, ansonsten "Eines existiert nicht!".

## Siehe auch

- [Fish Shell Dokumentation zu `test`](https://fishshell.com/docs/current/cmds/test.html)
- [Dokumentation des Betriebssystems zum `test`-Befehl](https://www.gnu.org/software/coreutils/manual/html_node/test-invocation.html)
- [Einführung in Fish Shell](https://fishshell.com/docs/current/tutorial.html)