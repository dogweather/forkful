---
title:                "Fish Shell: Ein Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Daten in der Vergangenheit oder Zukunft kann in verschiedenen Situationen nützlich sein, zum Beispiel bei der Planung von Terminen oder der Überprüfung von Zeitangaben in einem Projekt. Mit der Fish Shell ist es möglich, diese Berechnungen einfach und präzise durchzuführen.

## Wie geht's

```Fish Shell``` bietet verschiedene integrierte Funktionen und Optionen, um Datum und Zeit zu berechnen. Zum Beispiel können wir mit der ```date``` Funktion das aktuelle Datum anzeigen lassen:

```
date
```

Das Ergebnis ist das Datum und die Zeit des heutigen Tages. Um ein Datum in der Zukunft zu berechnen, können wir ein bestimmtes Datum im folgenden Format angeben:

```
date -d "3 days"
```

Dies gibt uns das Datum in drei Tagen. Wir können auch verschiedene Einheiten wie Wochen, Monate oder Jahre verwenden, wie in diesem Beispiel:

```
date -d "1 year 3 months"
```

Dies gibt uns das aktuelle Datum plus ein Jahr und drei Monate.

Zusätzlich gibt es auch die Option, ein bestimmtes Datum anhand eines festgelegten Formats zu berechnen. Zum Beispiel können wir das Datum in einer Woche im Format "Tag.Monat.Jahr" berechnen:

```
date -d "next week" +"%d.%m.%y"
```

Dies gibt uns das Datum im angegebenen Format, also zum Beispiel 12.09.19.

Weitere nützliche Funktionen und Optionen für die Berechnung von Datum und Zeit findest du in der offiziellen [Fish Shell Dokumentation](https://fishshell.com/docs/current/cmds/date.html).

## Tiefergehende Informationen

Das Berechnen von Datum und Zeit kann auch komplexer sein, wenn zum Beispiel unterschiedliche Zeitzonen oder Schaltjahre berücksichtigt werden müssen. In solchen Fällen bietet die Fish Shell weitere Optionen, um die Berechnungen genau anzupassen.

Um zum Beispiel eine andere Zeitzone zu verwenden, können wir die Option ```-u``` zusammen mit dem gewünschten Zeitzonen-Code angeben, zum Beispiel ```EST``` für die östliche Standardzeit. So können wir überprüfen, wie spät es an einem bestimmten Datum in einer anderen Zeitzone sein wird:

```
date -d "10pm next wednesday -u EST"
```

Natürlich ist es auch möglich, mit diesen Funktionen und Optionen komplexe Skripte zu schreiben, die verschiedene Berechnungen und Bedingungen beinhalten.

## Sieh auch

Hier sind einige weitere nützliche Ressourcen, um noch mehr über die verschiedenen Möglichkeiten der Fish Shell in Bezug auf das Berechnen von Datum und Zeit zu erfahren:

- [Fish Shell Dokumentation über Date & Time](https://fishshell.com/docs/current/cmds/date.html)
- [Fish Shell Tutorials auf Deutsch](https://www.das-labor.org/wiki/Fish_Anfänger_Tutorial)
- [Stack Overflow Fragen zum Thema Fish Shell](https://stackoverflow.com/questions/tagged/fish)

Wir hoffen, dass dieser Artikel hilfreich für dich war und wünschen dir viel Spaß beim Entdecken und Nutzen der vielfältigen Möglichkeiten der Fish Shell.