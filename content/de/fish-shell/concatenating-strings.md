---
title:                "Fish Shell: Verkettung von Zeichenfolgen"
simple_title:         "Verkettung von Zeichenfolgen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Eine der Grundfunktionen der Programmierung ist die Möglichkeit, Strings zu kombinieren, um damit komplexe Ausgaben zu erstellen. Das kann hilfreich sein, um zum Beispiel Benutzereingaben mit statischen Texten zu verbinden oder Daten aus verschiedenen Variablen zu kombinieren.

## Wie geht das?

Die Fish Shell bietet eine einfache Möglichkeit, Strings mit dem Befehl `string join` zu verbinden. Dabei wird der Befehl mit zwei Argumenten aufgerufen: dem Trennzeichen und einer Liste von Strings, die kombiniert werden sollen. Hier ist ein Beispiel:

```Fish Shell
string join " " "Hallo" "welt!" # Ausgabe: Hallo welt!
```

Wie man sehen kann, wird das Trennzeichen, hier ein Leerzeichen, zwischen den einzelnen Strings platziert. Man kann auch mehr als zwei Strings miteinander verbinden und verschiedene Trennzeichen verwenden. Hier ist ein weiteres Beispiel:

```Fish Shell
string join " - " "Ich" "bin" "eine" "Fish" "Shell" # Ausgabe: Ich - bin - eine - Fish - Shell
```

## Tiefergehende Informationen

Der Befehl `string join` kann auch noch mehr als nur die Verbindung von Strings. Man kann zum Beispiel auch Arrays verwenden, um damit dynamisch Strings zu verbinden. Hier ein Beispiel:

```Fish Shell
set fruits "Apfel" "Birne" "Banane"
string join " und " $fruits # Ausgabe: Apfel und Birne und Banane
```

Außerdem bietet die Fish Shell auch eine spezielle Syntax, um Strings zu kombinieren: den `..` Operator. Dieser funktioniert ähnlich wie `string join`, ist aber kürzer und kann auch andere Arten von Sequenzen miteinander verbinden. Hier ein Beispiel:

```Fish Shell
echo "Zahl: " 1..10 # Ausgabe: Zahl: 1 2 3 4 5 6 7 8 9 10
```

Eine detaillierte Beschreibung aller Möglichkeiten und Syntaxen gibt es in der offiziellen Fish Shell Dokumentation.

## Siehe auch

- Offizielle Fish Shell Dokumentation: https://fishshell.com/docs/current
- Einführung in die Fish Shell: https://opensource.com/article/18/5/how-learn-fish-shell
- Interaktives Tutorial für Fish Shell: https://rootnroll.com/d/fish-shell/
- Weitere Tipps und Tricks für die Fish Shell: https://www.example.com/