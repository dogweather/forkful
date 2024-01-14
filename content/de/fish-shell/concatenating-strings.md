---
title:    "Fish Shell: Verketten von Zeichenketten"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Warum
Beim Programmieren mit der Fish Shell ist es oft notwendig, verschiedene Zeichenfolgen miteinander zu verbinden oder zu kombinieren. Dies kann nützlich sein, um beispielsweise Dateipfade zu erstellen oder Texte zu formatieren.

## Wie geht man vor
Um Zeichenfolgen in der Fish Shell zu verbinden, gibt es die Möglichkeit, den Befehl `string join` zu verwenden. Dieser Befehl nimmt zwei oder mehr Zeichenfolgen als Argumente und fügt sie zu einer einzelnen Zeichenfolge zusammen. Hier ein Beispiel:

```Fish Shell
string join /home/user Documents
```
Ausgabe: `/home/user/Documents`

Man kann auch Variablen in den Befehl `string join` einbinden, um dynamische Zeichenfolgen zu erstellen. Hier ein Beispiel:

```Fish Shell
set name John
set age 30
string join "My name is" $name "and I am" $age "years old."
```
Ausgabe: `My name is John and I am 30 years old.`

Man kann auch eine Liste von Zeichenfolgen verwenden und diese mit dem Befehl `string repeat` wiederholt zusammenfügen. Hier ein Beispiel:

```Fish Shell
string join (string repeat "Hello " 3)
```
Ausgabe: `Hello Hello Hello `

## Tiefergehende Informationen
Beim Verbinden von Zeichenfolgen ist es wichtig, auf die richtige Reihenfolge der Argumente zu achten, um das gewünschte Ergebnis zu erzielen. Auch die Verwendung von Anführungszeichen kann wichtig sein, um Leerzeichen zwischen den einzelnen Zeichenfolgen zu setzen.

Außerdem kann man verschiedene String-Funktionen wie `string contains` oder `string split` verwenden, um komplexe Aufgaben mit Zeichenfolgen zu bewältigen.

## Siehe auch
- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell Tutorial auf Deutsch](https://fishshell.com/docs/current/index_de.html)
- [Weitere Beispiele und Tutorials zur Fish Shell](https://fishshell.com/docs/current/tutorials.html)