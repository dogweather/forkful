---
title:    "Fish Shell: Unterstrings extrahieren"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit der Extraktion von Teilstrings beschäftigen? Nun, manchmal braucht man nur einen Teil eines Textes oder einer Variable. Stellen Sie sich zum Beispiel vor, Sie haben einen Dateinamen und wollen nur die Dateiendung extrahieren. Oder Sie möchten eine URL in ihre verschiedenen Teile aufteilen. In diesen Fällen ist die Extraktion von Teilstrings eine nützliche Fähigkeit, die Ihnen viel Zeit und Mühe sparen kann.

## Anleitung

Um Teilstrings in Fish Shell zu extrahieren, verwenden wir den Befehl `string sub` gefolgt von der Position des gewünschten Teils zwischen Klammern. Schauen wir uns ein Beispiel an:

```fish
set filename "dokument.txt"
echo (string sub - 4 $filename)
```

Die Ausgabe dieses Codes wäre `txt`, da wir mit `string sub` die letzten 4 Zeichen des Strings `dokument.txt` extrahieren.

Für noch mehr Kontrolle können wir auch die Position und die Anzahl der Zeichen angeben, die wir extrahieren wollen. Hier ist ein Beispiel dafür:

```fish
set sentence "Hallo, wie geht es dir?"
echo (string sub 7 3 $sentence)
```

Dies würde `wei` ausgeben, da wir ab der 7. Position (bei Leerzeichen beginnt die Nummerierung bei 1) 3 Zeichen extrahieren.

## Tiefer Einblick

Die `string sub` Funktion ist Teil der Fish Shell Standardbibliothek und unterstützt auch Regex-Muster für die Positionierung. Sie können auch negative Zahlen verwenden, um Teilstrings von hinten zu extrahieren. Wenn Sie mehr darüber erfahren möchten, können Sie in der Fish Shell Dokumentation nachlesen.

## Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/cmds/string-sub.html)
- [Regex-Tutorial für Anfänger](https://regexone.com/) (auf Englisch)
- [Fish Shell Spezifikationen und Best Practices](https://fishshell.com/docs/current/index.html)