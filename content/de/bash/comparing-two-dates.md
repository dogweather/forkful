---
title:    "Bash: Zwei Daten vergleichen"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten in Bash kann hilfreich sein, um herauszufinden, welches Datum früher oder später liegt oder um zu überprüfen, ob ein bestimmtes Datum bereits vergangen ist.

## Anleitung

Um zwei Daten in Bash zu vergleichen, können wir die `date` Funktion verwenden. Zunächst müssen wir sicherstellen, dass wir die Daten im richtigen Format haben. Anschließend können wir die `date` Funktion verwenden, um die Daten in ein Timestamp umzuwandeln. Diesen können wir dann einfach miteinander vergleichen.

```Bash
date1="2020-10-15" # erster Datumsstring
date2="2020-10-23" # zweiter Datumsstring

date1_timestamp=`date -d "$date1" +%s` # wandele erstes Datum in Timestamp um
date2_timestamp=`date -d "$date2" +%s` # wandele zweites Datum in Timestamp um

if [ $date1_timestamp -lt $date2_timestamp ]; then # vergleiche die Timestamps
    echo "$date1 ist früher als $date2"
elif [ $date1_timestamp -gt $date2_timestamp ]; then
    echo "$date1 ist später als $date2"
else
    echo "Beide Daten sind gleich"
fi
```

Das obige Beispiel gibt den Text "2020-10-15 ist früher als 2020-10-23" aus.

## Tiefergehende Informationen

Um zwei Daten in Bash zu vergleichen, müssen sie in das Format "Jahr-Monat-Tag" gebracht werden. Hierfür kann entweder die `date` Funktion oder der Befehl `$(date +"%Y-%m-%d")` verwendet werden. Der `%s` Parameter bei der `date` Funktion wird verwendet, um die Daten in einen Timestamp umzuwandeln, der dann für den Vergleich verwendet werden kann.

Es ist auch möglich, die Daten in einem anderen Format zu vergleichen, jedoch muss dann die Syntax entsprechend angepasst werden.

## Siehe auch

- [Bash-Befehlsreferenz](https://www.gnu.org/software/bash/manual/html_node/Bash-Date-Declarations.html)
- [Date-Manpage](https://linux.die.net/man/1/date)