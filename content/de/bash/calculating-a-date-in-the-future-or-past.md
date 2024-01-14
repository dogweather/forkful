---
title:    "Bash: Berechnen eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Daten in der Zukunft oder Vergangenheit kann eine sehr hilfreiche Aufgabe in der Bash-Programmierung sein. Es ermöglicht es uns, automatisierte Aufgaben zu erstellen, die auf bestimmte zeitliche Ereignisse reagieren oder uns einfach dabei helfen, wichtige Termine im Voraus zu planen.

## Wie geht das?

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, gibt es in Bash verschiedene Möglichkeiten. Eine häufig genutzte Methode ist die Verwendung des Befehls "date". Dieser Befehl kann verwendet werden, um verschiedene Zeiteinheiten wie Tage, Monate oder Jahre zu einer bestimmten Datumsstempel hinzuzufügen oder davon abzuziehen.

Hier ist ein Beispiel, wie wir den Befehl "date" verwenden können, um das Datum von heute um 7 Tagen in der Zukunft zu berechnen:

```Bash
future_date=$(date +'%d-%m-%Y' -d "+7 days")
echo "Das Datum in 7 Tagen wird sein: $future_date"
```

Die Ausgabe dieses Codes wird das Datum in der Zukunft angeben: "03-12-2019".

Um ein Datum in der Vergangenheit zu berechnen, können wir den Befehl "date" ebenfalls verwenden, jedoch mit einem negativen Wert für die Zeiteinheit. Zum Beispiel, um das Datum von heute vor 7 Tagen zu erhalten, können wir folgenden Code verwenden:

```Bash
past_date=$(date +'%d-%m-%Y' -d "-7 days")
echo "Das Datum vor 7 Tagen war: $past_date"
```

Die Ausgabe wird in diesem Fall das Datum in der Vergangenheit angeben: "19-11-2019".

## Tiefer Einblick

Die Verwendung des Befehls "date" ist nur eine Möglichkeit, um ein Datum in der Zukunft oder Vergangenheit zu berechnen. Wir können auch die "dateutils" nutzen, eine Sammlung von Tools speziell für die Arbeit mit Datumsangaben in Bash.

Ein weiteres hilfreiches Tool ist der Befehl "cal", der einen Kalender für einen bestimmten Monat oder ein ganzes Jahr anzeigen kann. Wir können auch den Befehl "date" mit "cal" kombinieren, um bestimmte Tage in einem bestimmten Monat oder Jahr zu erhalten. Zum Beispiel, um den Dienstag in der letzten Woche des nächsten Monats zu erhalten, können wir folgenden Code verwenden:

```Bash
tuesday=$(date +'%d' -d "next month" -d "Sunday-1 Week")
cal -m $(date +'%m %Y' -d "next month") | grep "$tuesday"
```

Die Ausgabe wird das Datum des Dienstags in der letzten Woche des nächsten Monats anzeigen, z.B. "31".

## Siehe auch

- Dokumentation des Befehls "date": https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Dateutils Dokumentation: https://www.fresse.org/dateutils/
- Weitere nützliche Bash Befehle: https://www.linuxnix.com/most-useful-linux-commands/