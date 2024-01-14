---
title:    "Fish Shell: Das aktuelle Datum erhalten"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals ein Skript geschrieben haben, das auf die aktuelle Zeit oder das aktuelle Datum zugreifen musste, werden Sie verstehen, warum es wichtig ist, das Datum in der Fish Shell abzurufen. Das Datum ist eine grundlegende und häufig verwendete Information in der Programmierung und kann in vielen Situationen nützlich sein.

## Wie es geht

Es gibt mehrere Möglichkeiten, in der Fish Shell auf das aktuelle Datum zuzugreifen. Eine Möglichkeit ist die Verwendung des `date`-Befehls. Geben Sie einfach `date` in der Shell ein und Sie erhalten das aktuelle Datum im Format "Wochentag Monat Tag Stunde:Minute: Sekunde Zeitzone Jahr". Sie können auch das `date`-Kommando mit verschiedenen Optionen verwenden, um das Format des Datums anzupassen.

```Fish Shell
$ date
Fri Feb 19 15:23:31 EST 2021
```

Eine weitere Möglichkeit besteht darin, die `strftime`-Funktion zu verwenden, um das Datum in einem bestimmten Format auszugeben. Hier ist ein Beispiel, das das Datum im Format "Tag.Monat.Jahr" ausgibt:

```Fish Shell
$ set datum (strftime "%d.%m.%y")
$ echo $datum
19.02.21
```

Eine weitere nützliche Funktion ist `now`, mit der Sie das aktuelle Datum und die Uhrzeit in verschiedenen Formaten abrufen können.

```Fish Shell
$ echo (now) # Gibt das aktuelle Datum und die Uhrzeit im ISO-Format aus
2021-02-19T20:23:31
$ echo (now '%H:%M') # Gibt die aktuelle Uhrzeit im Format Stunden:Minute aus
20:23
```

## Tiefere Einblicke

Es gibt noch viel mehr Möglichkeiten, in der Fish Shell auf das aktuelle Datum zuzugreifen und es zu formatieren. Sie können beispielsweise die `tzselect`-Funktion verwenden, um die Zeitzone für das Datum festzulegen. Sie können auch Benutzervariablen verwenden, um das Datum in Ihrem Skript zu speichern und zu verwenden.

Das aktuelle Datum ist auch nützlich, wenn Sie Skripte schreiben, die bestimmte Aktionen an bestimmten Tagen oder zu bestimmten Zeiten ausführen sollen. Sie können das Datum mit Bedingungen und Schleifen kombinieren, um komplexe Skripte zu erstellen.

## Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Tutorial: Shell-Scripting im Fish Shell](https://dev.to/bameyrick/shell-scripting-in-fish-a-tutorial-2mek)
- [Offizieller Fish Shell GitHub-Repository](https://github.com/fish-shell/fish-shell)