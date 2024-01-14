---
title:    "Haskell: In German, the translated title would be Das Verketten von Strings."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Warum

Stellen Sie sich vor, Sie haben eine Liste von Namen und möchten diese zu einem vollständigen Satz verbinden. Oder vielleicht müssen Sie eine E-Mail-Nachricht mit dynamischen Inhalten erstellen. In diesen Situationen ist die Verkettung von Zeichenfolgen eine nützliche Funktion, die Ihnen hilft, diese Aufgaben effizient zu erledigen. In diesem Blogbeitrag werden wir uns genauer ansehen, wie wir in Haskell Zeichenfolgen verketten können.

## Wie

Im Folgenden finden Sie einen Beispielcode zum Verketten von zwei Zeichenfolgen in Haskell:

```Haskell
let vorname = "Lisa"
let nachname = "Müller"
let vollerName = vorname ++ " " ++ nachname
putStrLn vollerName
```

Das Ergebnis dieses Codes ist "Lisa Müller". Wie Sie sehen, verwenden wir den Operator "++" (Konkatenation) zwischen zwei Zeichenfolgen, um sie zu verbinden. Sie können diesen Operator auf beliebig viele Zeichenfolgen anwenden, um eine lange Kette zu erstellen.

## Deep Dive

Der Operator "++" ist ein Beispiel dafür, wie Haskell String-Verkettung unterstützt. Es gibt jedoch andere Möglichkeiten, dies zu erreichen, wie z.B. die Verwendung der Funktion "concat". Diese Funktion nimmt eine Liste von Zeichenfolgen entgegen und verbindet sie zu einer einzigen Zeichenfolge. Sie können auch die Funktion "foldl" verwenden, um eine Liste von Zeichenfolgen mit einem bestimmten Trennzeichen zu verketten. Diese Techniken können nützlich sein, wenn Sie mit komplexeren Datenstrukturen arbeiten.

Ein wichtiger Punkt zu beachten ist, dass Haskell Zeichenfolgen als Liste von Zeichen betrachtet. Das bedeutet, dass die Verkettung von Zeichenfolgen effektiv das Hinzufügen von Elementen zur Liste ist. Dies kann bei sehr langen Zeichenfolgen zu einer schlechten Performance führen, da jedes Hinzufügen an das Ende der Liste eine Traversierung der gesamten Liste erfordert. In solchen Fällen können Sie die Funktion "Data.Text.concat" aus dem Modul Data.Text verwenden, die eine bessere Performance bietet.

# Siehe auch

- [Haskell-Dokumentation zur String-Verkettung](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-String.html#g:2)
- [Beispielcode zum Verketten von Strings in Haskell](https://wiki.haskell.org/String_concatenation)
- [Einführung in Haskell für Anfänger](https://haskell.org/)