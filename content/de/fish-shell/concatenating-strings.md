---
title:                "Fish Shell: Zusammenführen von Zeichenketten"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum
Wenn du dich mit der Fish Shell beschäftigst, wirst du früher oder später auf das Konzept des Zusammenfügens von Zeichenfolgen oder auch "String Concatenation" stoßen. Dies ist eine grundlegende Funktion, die dir dabei hilft, Zeichenfolgen zu manipulieren und zusammenzufügen, um komplexere Aktionen auszuführen. In diesem Blogbeitrag werden wir uns genauer mit dem Warum, dem Wie und den Hintergründen von String Concatenation in der Fish Shell beschäftigen.

## Wie geht das?
Um Zeichenfolgen in der Fish Shell zu konkatenieren, verwenden wir den Punkt-Operator (.). Dieser Operator verbindet zwei Zeichenfolgen miteinander und gibt eine kombinierte Zeichenfolge zurück. Hier ist ein Beispiel:

```
Fish Shell:

set name "Max"
set age "25"

echo $name . " ist " . $age . " Jahre alt"

output: Max ist 25 Jahre alt
```

Wie du sehen kannst, haben wir zuerst zwei Variablen mit dem Befehl `set` erstellt und dann mit dem Punkt-Operator den Inhalt der Variablen zusammengefügt. Es ist wichtig zu beachten, dass der Punkt-Operator nur Strings miteinander verbinden kann, daher müssen wir gegebenenfalls Zahlen in Strings umwandeln, um sie hinzuzufügen.

Eine weitere Möglichkeit, Zeichenfolgen zu konkatenieren, ist die Verwendung von Platzhaltern. Hier ist ein Beispiel:

```
Fish Shell:

set fruit "Apfel"

echo "Ich esse gerne %s" $fruit

output: Ich esse gerne Apfel
```

Der Platzhalter %s wird durch den Wert der Variable `fruit` ersetzt. Dies ist besonders nützlich, wenn man komplexe Zeichenfolgen mit variablen Werten zusammenfügen möchte, da es viel übersichtlicher und weniger fehleranfällig ist.

## Tiefergehende Informationen
In der Fish Shell gibt es auch die Möglichkeit, mehrere Zeichenfolgen miteinander zu verketten. Dabei werden alle Zeichenfolgen hintereinandergehängt und ergeben eine große Zeichenfolge. Hier ist ein Beispiel:

```
Fish Shell:

set string1 "Hallo"
set string2 "Welt"

echo $string1$string2

output: HalloWelt
```

Eine weitere wichtige Funktion ist die Verwendung von Variablen innerhalb des Punkt-Operators. Hier ist ein Beispiel:

```
Fish Shell:

set fruit "Bananen"

set statement "Ich esse gerne"

echo $statement . " " . $fruit

output: Ich esse gerne Bananen
```

Wie du sehen kannst, können wir Variablen sowohl vor als auch nach dem Punkt-Operator verwenden, um komplexe Zeichenfolgen zu erstellen.

## Siehe auch
- [Official Fish Shell documentation on string concatenation](https://fishshell.com/docs/current/tutorial.html#tut_concat)
- [Fish Shell scripting tutorial on string manipulation](https://github.com/jorgebucaran/fisher)
- [Blog post about advanced string concatenation techniques in Fish Shell](https://blog.fishshell.com/fish-shellie/)