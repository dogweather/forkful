---
title:                "Bash: Ausgabe von Debug-Informationen drucken"
simple_title:         "Ausgabe von Debug-Informationen drucken"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debug-Ausgabe ist ein wichtiger Teil des Programmierens, da sie bei der Identifizierung und Behebung von Fehlern in Ihrem Code hilft. Durch das Drucken von Nachrichten und Variablenwerten während der Ausführung Ihres Codes können Sie besser verstehen, was in Ihrem Programm passiert und wo der Fehler liegt. Es ist ein effektiver Weg, um Ihre Codequalität zu verbessern und Probleme schneller zu lösen.

## Wie man es macht

Die Ausgabe von Debug-Nachrichten in Bash ist einfach und kann mit dem `echo` Befehl durchgeführt werden. Hier ist ein Beispiel, um eine Nachricht auszugeben, die bestätigt, dass ein bestimmter Teil des Codes erfolgreich ausgeführt wurde:

```Bash
echo "Der Code wurde erfolgreich ausgeführt!"
```

Sie können auch Variablenwerte ausgeben, um zu überprüfen, ob diese den erwarteten Wert haben. Zum Beispiel:

```Bash
name="Max"
echo "Mein Name ist $name."
```

Die Ausgabe erscheint dann folgendermaßen:

```
Mein Name ist Max.
```

Wenn Sie mehrere Variablen ausgeben möchten, können Sie das `printf` Befehl verwenden. Dies ist besonders nützlich, wenn Sie komplexe Variablenwerte haben, die formatiert werden müssen. Hier ist ein Beispiel, um den Wert von zwei Variablen auszugeben:

```Bash
age=28
balance=1000.50
printf "Ich bin %d Jahre alt und mein Kontostand beträgt %.2f Euro." $age $balance
```

Die Ausgabe lautet dann:

```
Ich bin 28 Jahre alt und mein Kontostand beträgt 1000.50 Euro.
```

## Tiefer eintauchen

Zusätzlich zu den grundlegenden Ausgabetechniken können Sie auch Formatierungsoptionen verwenden, um die Ausgabe Ihrer Debug-Nachrichten zu verbessern. Diese können in der offiziellen Bash-Dokumentation gefunden werden. Sie können auch benutzerdefinierte Funktionen erstellen, die das Debugging erleichtern, indem Sie die Ausgabe mit zusätzlichen Informationen wie Zeitstempel und Dateinamen versehen. Eine andere Möglichkeit ist die Verwendung des `set -x` Befehls, um einen Shell-Trace-Modus zu aktivieren, der alle ausgeführten Befehle anzeigt.

## Siehe auch

- Offizielle Bash-Dokumentation: https://www.gnu.org/software/bash/manual/
- Benutzerdefinierte Debug-Funktionen: https://emdentec.com/blog/bash-debug-german/
- Shell-Trace-Modus: https://ss64.com/bash/set.html