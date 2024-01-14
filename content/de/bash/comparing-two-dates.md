---
title:                "Bash: Vergleich von zwei Daten"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

Das Vergleichen von zwei Daten ist eine häufige Aufgabe in der Bash-Programmierung. Es ermöglicht uns, Daten zu filtern, zu sortieren oder zu überprüfen, ob ein bestimmtes Ereignis in der Vergangenheit liegt. In diesem Blog-Beitrag erfahren Sie, warum es wichtig ist, diese Fähigkeit zu beherrschen und wie Sie es in Ihrem Bash-Skript implementieren können.

## How To

In der Bash-Programmierung gibt es verschiedene Möglichkeiten, zwei Daten miteinander zu vergleichen. Die einfachste Möglichkeit besteht darin, den `test` Befehl zu verwenden und die Daten mithilfe von Vergleichsoperatoren zu vergleichen. Zum Beispiel können wir überprüfen, ob eine Datei älter ist als eine andere Datei, indem wir den Ausdruck `if [ datei1 -ot datei2 ]; then` verwenden. Hier zeigt das `-ot` Zeichen den Vergleich "älter als" an.

```Bash
if [ datei1 -ot datei2 ]; then
    echo "Datei1 ist älter als Datei2."
else
    echo "Datei2 ist älter als Datei1."
fi
```

Eine andere Möglichkeit besteht darin, die Daten mithilfe des `date` Befehls in ein einheitliches Format zu bringen und dann miteinander zu vergleichen. Dies gibt uns mehr Flexibilität bei der Formatierung der Daten. Zum Beispiel können wir überprüfen, ob ein bestimmter Monat in der Zukunft liegt, indem wir den Ausdruck `if [ $(date -d "2020-12-01" +%m) -gt $(date +%m) ] ; then` verwenden.

```Bash
if [ $(date -d "2020-12-01" +%m) -gt $(date +%m) ] ; then
    echo "Dezember liegt in der Zukunft."
else
    echo "Wir sind bereits im Dezember."
fi
```

## Deep Dive

Beim Vergleichen von Daten ist es wichtig zu beachten, dass nicht alle Formate direkt verglichen werden können. Zum Beispiel können wir nicht einfach zwei Datumsangaben im Format "Tag-Monat-Jahr" miteinander vergleichen, da dies nicht der natürlichen Reihenfolge entspricht. In solchen Fällen müssen wir die Daten in ein einheitliches Format bringen, z. B. das Unix-Timestamp-Format, bevor wir sie vergleichen können.

Ein weiterer wichtiger Aspekt ist die Verwendung von Variablen beim Vergleichen von Daten. Wir müssen sicherstellen, dass die Variablen die richtigen Werte enthalten und dass die Vergleichsoperatoren korrekt angewendet werden. Sonst kann dies zu unerwarteten Ergebnissen führen.

## Siehe auch

- [Bash - The GNU Project](https://www.gnu.org/software/bash/)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/index.html)
- [Linux Commands - ss64.com](https://ss64.com/bash/)