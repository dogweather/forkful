---
title:                "Bash: Zufällige Zahlen generieren"
simple_title:         "Zufällige Zahlen generieren"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Hast du dich jemals gefragt, wie Computer zufällige Zahlen generieren? Oder brauchst du vielleicht zufällige Zahlen für ein bestimmtes Programm? In diesem Blogbeitrag werden wir uns genau damit beschäftigen: Wie man in Bash zufällige Zahlen generiert.

## Wie geht man vor?

Um zufällige Zahlen in Bash zu generieren, können wir die integrierte Funktion `echo $RANDOM` verwenden. Diese Funktion gibt eine zufällige Zahl zwischen 0 und 32767 aus. Hier ist ein Beispiel:

```Bash
echo $RANDOM
```

Die Ausgabe könnte beispielsweise `16574` sein. Wenn wir jedoch mehrere zufällige Zahlen benötigen, können wir eine Schleife verwenden, um die Funktion mehrmals auszuführen:

```Bash
for i in {1..5}; do
    echo $RANDOM
done
```

Dieser Code wird fünf zufällige Zahlen ausgeben, jeweils in einer neuen Zeile. Wir können auch den Bereich der generierten Zahlen einschränken, indem wir einen Wert zwischen `0` und `32767` multiplizieren und anschließend den gewünschten Bereich addieren. Zum Beispiel:

```Bash
# Generiere eine zufällige Zahl von 1 bis 10
echo $(($RANDOM % 10 + 1))
```

Die Ausgabe könnte beispielsweise `7` sein.

## Tiefere Einblicke

Es gibt auch die Möglichkeit, zufällige Zeichenfolgen in Bash zu generieren. Dazu können wir die Befehlszeile `head` in Kombination mit dem zufälligen Textgenerator `base64` verwenden. Hier ist ein Beispiel:

```Bash
# Generiere eine zufällige Zeichenfolge mit 10 Zeichen
head /dev/urandom | tr -dc 'a-zA-Z0-9' | head -c10 | base64
```

Die Ausgabe könnte beispielsweise `sZdZGRhLW0=` sein. Indem wir die Anzahl der generierten Zeichen in `head -c` anpassen, können wir auch längere Zeichenfolgen erzeugen.

Es gibt viele andere Möglichkeiten, zufällige Zahlen und Zeichenfolgen in Bash zu generieren. Mit ein wenig Experimentieren und einigen verschiedenen Funktionen kannst du den Ausgabeumfang und die Vielfalt der zufälligen Zahlen und Zeichenfolgen steuern.

## Siehe auch

- [Bash-Dokumentation](https://www.gnu.org/software/bash/manual/bash.html)
- [Weitere Tipps zum Generieren von Zufallszahlen in Bash](https://linuxconfig.org/random-numbers-generation-with-bash-and-sys-dev-random)
- [Schleifen in Bash](https://linuxize.com/post/bash-for-loop/)