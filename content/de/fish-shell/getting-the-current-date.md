---
title:                "Fish Shell: Das aktuelle Datum erhalten"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum

Es gibt viele Gründe, warum man das aktuelle Datum in seinem Fish Shell Programm nutzen möchte. Vielleicht möchtest du es als Teil eines Dateinamens verwenden oder als Teil einer automatisierten Backups-Routine. Egal aus welchem Grund, das Erlernen des Befehls zur Abfrage des aktuellen Datums kann deine Programmierfähigkeiten erweitern und dein Fish Shell-Erlebnis verbessern.

# Wie geht das?

Um das aktuelle Datum im Fish Shell zu erhalten, musst du den Befehl `date` verwenden. Dieser Befehl akzeptiert verschiedene Formatierungsoptionen, um das Datum in deiner gewünschten Form anzuzeigen. Schauen wir uns einige Beispiele an:

```Fish Shell
date +"%d.%m.%Y"
```
Dieser Befehl gibt das aktuelle Datum im Format "Tag.Monat.Jahr" aus. Zum Beispiel `21.05.2021`.

```Fish Shell
date +"%A"
```
Diese Zeile gibt den aktuellen Wochentag aus, z.B. `Freitag`.

Es gibt viele verschiedene Formatierungsoptionen, die du ausprobieren kannst. Um eine vollständige Liste der verfügbaren Optionen zu sehen, kannst du den Befehl `man date` eingeben, um die man-Seite des Datei-Befehls aufzurufen.

# Tiefere Einblicke

Wenn du einen tieferen Einblick in die Welt der Datumsmethoden im Fish Shell erhalten möchtest, kannst du das `strftime`-Tool verwenden. Dieses Tool ermöglicht es dir, eigene benutzerdefinierte Datumsformate zu erstellen. Zum Beispiel:

```Fish Shell
echo (strftime "%d. %B %Y" (language "de") (date))
```

Dieser Befehl nutzt die strftime-Funktion, um das aktuelle Datum im Format "Tag. Monat Jahr" auszugeben, wobei der Monat in deutscher Sprache angezeigt wird. Du kannst auch andere Sprachen ausprobieren, indem du den entsprechenden Sprachcode anstelle von "de" verwendest.

# Sieh dir auch an

- Offizielle Fish Shell Dokumentation für den `date` Befehl: [https://fishshell.com/docs/current/cmds/date.html](https://fishshell.com/docs/current/cmds/date.html)
- Manpage für den `strftime`-Befehl: [https://fishshell.com/docs/current/cmds/strftime.html](https://fishshell.com/docs/current/cmds/strftime.html)
- Eine Liste von verfügbaren Sprachcodes für die strftime-Funktion: [https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes](https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes)