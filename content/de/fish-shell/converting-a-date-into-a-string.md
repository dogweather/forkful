---
title:                "Fish Shell: Ein Datum in einen String umwandeln"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man ein Datum in eine Zeichenkette umwandeln möchte. Eine häufige Anwendung ist zum Beispiel die Ausgabe von Datumsangaben in einem bestimmten Format, das besser lesbar oder besser an die Anforderungen des Projekts angepasst ist.

## Wie geht es

Um ein Datum in eine Zeichenkette umzuwandeln, gibt es verschiedene Ansätze, je nachdem welche Programmiersprache oder Framework man verwendet. Im Folgenden wird gezeigt, wie man dies in der Fish Shell erreichen kann.

```Fish Shell
set -l date (date -f "%Y-%m-%d") 
echo "Das heutige Datum ist $date."
```

Dieser Code verwendet den Befehl `date` mit dem Parameter `-f`, um das Datum in das gewünschte Format zu bringen. In diesem Fall wird `%Y-%m-%d` verwendet, was für das aktuelle Jahr, Monat und Tag steht. Die Ausgabe wird dann in der Variablen `date` gespeichert und anschließend mit dem Befehl `echo` ausgegeben.

Die Ausgabe dieses Codes sieht wie folgt aus:

```
Das heutige Datum ist 2020-09-24.
```

## Tiefer eintauchen

Die Fish Shell enthält auch eine integrierte Funktion `strftime`, um ein Datum in eine Zeichenkette umzuwandeln. Hier ein Beispiel:

```Fish Shell
set -l date (strftime "%A, %e %B %Y")
echo "Heute ist $date."
```

Die Funktion `strftime` verwendet für das Datum das Format `%A, %e %B %Y`, was für den aktuellen Wochentag, den Tag im Monat ausgeschrieben, den Monat des Jahres ausgeschrieben und das gesamte Jahr steht.

Die Ausgabe dieses Codes sieht wie folgt aus:

```
Heute ist Donnerstag, 24 September 2020.
```

Es gibt noch viele weitere Formatierungsoptionen für die Funktion `strftime`, die je nach Bedarf angepasst werden können. Eine vollständige Liste findet man in der [Fish Shell Dokumentation](https://fishshell.com/docs/current/cmds/strftime.html).

## Siehe auch

- [Offizielle Fish Shell Dokumentation](https://fishshell.com/docs/current/)
- [Fish Shell Cheatsheet in Deutsch](https://github.com/fish-shell/fish-shell/blob/master/doc/info/fish.info)
- [Übersicht über die wichtigsten Fish Shell Befehle](https://digitalocean.com/community/tutorials/an-introduction-to-fish-a-smart-and-user-friendly-command-line-shell)