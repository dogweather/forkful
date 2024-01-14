---
title:    "Bash: Verkettung von Zeichenketten"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

# Warum
Das Zusammenfügen oder Verketten von Zeichenfolgen ist ein wichtiger Bestandteil der Bash-Programmierung. Es ermöglicht Ihnen, verschiedene Texte oder Variablen miteinander zu verbinden und so komplexe Ausgaben oder Bedingungen zu erstellen. In diesem Blogbeitrag werden wir uns genauer damit befassen, wie man Strings in Bash verketten kann.

## Wie geht das?
Das Verketten von Strings in Bash ist sehr einfach und erfordert keine komplizierten Schritte. Zunächst müssen Sie die einzelnen Zeichenfolgen definieren, die Sie miteinander verbinden möchten. Dies kann durch die Verwendung von einfachen Anführungszeichen ```'``` oder doppelten Anführungszeichen ```"``` geschehen.

```Bash
text1='Hallo'
text2="Welt"
```

Anschließend können Sie die beiden Strings mit dem Operator ```+``` verketten und das Ergebnis einer neuen Variablen zuweisen.

```Bash
ergebnis=$text1+$text2
echo $ergebnis
```

Der Befehl ```echo``` gibt nun die verknüpfte Zeichenfolge aus, in diesem Fall "Hallo+Welt". Sie können natürlich auch mehr als zwei Strings miteinander verketten.

## Tiefer eintauchen
Das Verketten von Strings in Bash ist nicht nur auf die Verwendung des Operators ```+``` beschränkt. Es gibt verschiedene Möglichkeiten, dies zu tun, z.B. mit der Funktion ```printf``` oder durch das Ausgeben von Variablen.

Eine interessante und nützliche Möglichkeit ist das Verwenden von geschweiften Klammern ```{}```. Diese ermöglichen es Ihnen, mehrere Zeichenfolgen oder Variablen zu einer einzelnen Zeichenfolge zu kombinieren.

```Bash
vorname='Max'
nachname='Mustermann'
echo "Mein Name ist ${vorname} ${nachname}."
```

Die Ausgabe ist nun "Mein Name ist Max Mustermann.". Beachten Sie, dass innerhalb der geschweiften Klammern keine Leerzeichen zwischen der Variablen und dem umgebenden Text vorhanden sein dürfen.

## Siehe auch
- [Bash String Concatenation](https://www.tutorialkart.com/bash-shell-scripting/bash-concatenate-strings/)
- [The Linux Documentation Project](https://www.tldp.org/LDP/abs/html/abs-guide.html#STREXMP)
- [Bash Guide for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)