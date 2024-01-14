---
title:    "Gleam: Verkettung von Zeichenfolgen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Zusammenfügen von Zeichenketten (auch bekannt als String-Konkatenation) ist eine grundlegende Fähigkeit, die in vielen Programmiersprachen benötigt wird. Es ermöglicht es Entwicklern, mehrere Strings zu einem einzigen String zu verbinden und so komplexe Texte zu erstellen. In diesem Artikel werden wir uns ansehen, wie man dies mit der Gleam-Programmiersprache erreichen kann.

## So geht's

In Gleam ist die Konkatenation von Zeichenketten ganz einfach. Wir verwenden dazu den Operator `++`, der zwei Strings miteinander verbindet.

```
Gleam> "Hallo, " ++ "Welt!"
"Hallo, Welt!"
```

In diesem Beispiel haben wir die Strings "Hallo, " und "Welt!" miteinander konkateniert, um den Satz "Hallo, Welt!" zu erstellen. Beachten Sie, dass zwischen den beiden Strings ein Leerzeichen eingefügt wurde, da wir dies nicht explizit angegeben haben.

Aber was passiert, wenn wir mehr als zwei Strings miteinander verbinden wollen? Dafür können wir einfach mehrere `++` Operatoren aneinanderreihen.

```
Gleam> "Guten " ++ "Morgen, " ++ "alle zusammen!"
"Guten Morgen, alle zusammen!"
```

Wie Sie sehen können, können wir so viele Strings miteinander verbinden, wie wir möchten, um komplexe Texte zu erstellen.

## Tiefer eintauchen

Obwohl die Technik der String-Konkatenation einfach ist, gibt es einige Dinge, die man berücksichtigen sollte. Zum Beispiel können wir nicht einfach einen String mit einem anderen Datentyp wie einer Zahl verbinden.

```
Gleam> "5" ++ 3
Cannot concatenate a string and an integer.
```

Um dies zu ermöglichen, müssen wir die Zahl zuerst in einen String umwandeln.

```
Gleam> "5" ++ Integer.to_string(3)
"53"
```

Darüber hinaus kann die Konkatenation von Strings auch bei der Verwendung von Variablen nützlich sein, um dynamischere Texte zu erstellen.

```
Gleam> name = "Hans"
Gleam> "Hallo, " ++ name ++ "!"
"Hallo, Hans!"
```

## Siehe auch

- [String-Konkatenation in C++](https://www.tutorialspoint.com/cplusplus/cpp_string_concatenation.htm)
- [String-Konkatenation in Java](https://www.javatpoint.com/concatenation-in-java)