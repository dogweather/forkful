---
title:                "Fish Shell: Erstellen einer temporären Datei"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum Sie möglicherweise temporäre Dateien in Ihrer Fish Shell-Programmierung erstellen müssen. Eine häufige Verwendung ist beispielsweise das Speichern von Zwischenergebnissen, die während der Ausführung Ihres Skripts benötigt werden. Temporäre Dateien können auch dazu dienen, Platz auf Ihrer Festplatte zu sparen, da sie automatisch gelöscht werden, sobald sie nicht mehr benötigt werden.

## So geht's

Um eine temporäre Datei in Fish Shell zu erstellen, können Sie den `mktemp` Befehl verwenden. Dieser Befehl generiert eine eindeutige, zufällig benannte Datei in Ihrem temporären Ordner. Sie können dann mit der Datei wie mit jeder anderen Datei arbeiten. Im folgenden Beispiel erstellen wir eine temporäre Datei, schreiben etwas Text in sie und geben den Inhalt aus:

```
Fish Shell Code:
set tempdatei (mktemp)
echo "Hallo Welt!" > $tempdatei
cat $tempdatei

Ausgabe:
Hallo Welt!
```
Wie Sie sehen können, können Sie auf die temporäre Datei mit der Variable `$tempdatei` zugreifen.

## Tiefer Einblick

Wenn Sie genauer verstehen möchten, wie das Erstellen von temporären Dateien in Fish Shell funktioniert, können Sie sich die `mktemp` Dokumentation ansehen. Dort finden Sie detaillierte Informationen über die verschiedenen Optionen und Verwendungsmöglichkeiten dieses Befehls.

## Siehe auch

- Fish Shell Dokumentation zu `mktemp`: https://fishshell.com/docs/current/index.html#command-mktemp
- Weitere Möglichkeiten, temporäre Dateien in Fish Shell zu erstellen: https://fishshell.com/docs/current/commands.html#tempfile
- Beispielprojekt für die Verwendung von temporären Dateien in Fish Shell: https://github.com/johndoe/fish-shell-tempfile-demo