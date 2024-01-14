---
title:                "Fish Shell: Eine Zeichenkette großschreiben"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

In diesem Artikel werden wir uns damit beschäftigen, wie man eine Zeichenfolge in Großbuchstaben konvertieren kann. Dies kann nützlich sein, wenn man beispielsweise einen Dateinamen oder einen Satz in einer bestimmten Formatierung benötigt.

## Wie es geht

Um eine Zeichenfolge in Großbuchstaben umzuwandeln, können wir die "`str toupper`" Funktion in Fish Shell verwenden. Hier ist ein Beispiel, wie wir dies tun könnten:

```Fish Shell
set string "Hallo Welt!"
echo $string # Output: Hallo Welt!
str toupper $string
echo $string # Output: HALLO WELT!
```

Wie Sie sehen können, haben wir zuerst eine Variable namens "string" erstellt und ihr den Wert "Hallo Welt!" zugewiesen. Dann haben wir die `str toupper` Funktion auf diese Variable angewendet und den resultierenden Wert ausgegeben.

Wir können auch mehrere Zeichenfolgen gleichzeitig in Großbuchstaben konvertieren, indem wir das "-Lb" Flag verwenden. Schauen wir uns ein weiteres Beispiel an:

```Fish Shell
set string1 "Ich bin ein Fisch!"
set string2 "Und ich auch!"
echo $string1 # Output: Ich bin ein Fisch!
echo $string2 # Output: Und ich auch!
str toupper -Lb string1 string2
echo $string1 # Output: ICH BIN EIN FISCH!
echo $string2 # Output: UND ICH AUCH!
```

## Tiefer Einblick

Wenn wir uns den Code der `str toupper` Funktion im Fish Shell genauer ansehen, werden wir feststellen, dass sie auf die `string` Bibliothek zugreift. Diese Bibliothek bietet verschiedene nützliche Funktionen für die Manipulation von Zeichenfolgen. Die `str toupper` Funktion selbst verwendet die `str fromlower` Funktion, um alle Kleinbuchstaben in Großbuchstaben zu konvertieren.

Es ist wichtig zu beachten, dass die `str toupper` Funktion nur ASCII-Zeichen unterstützt. Das bedeutet, dass Zeichen aus anderen Sprachen oder Sonderzeichen nicht in Großbuchstaben konvertiert werden.

Insgesamt ist die `str toupper` Funktion eine einfache und praktische Möglichkeit, Zeichenfolgen in Großbuchstaben zu konvertieren, aber sie hat auch ihre Grenzen. Wenn Sie eine umfassendere und flexiblere Lösung benötigen, können Sie sich andere Funktionen in der `string` Bibliothek ansehen oder sogar eine benutzerdefinierte Funktion erstellen.

## Siehe auch

- Fish Shell Dokumentation zu "str toupper": https://fishshell.com/docs/current/cmds/str.html#toupper
- Fish Shell Dokumentation zu "`string`" Bibliothek: https://fishshell.com/docs/current/cmds/string.html