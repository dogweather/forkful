---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die String-Interpolation ist ein Weg, um Variablenwerte innerhalb von Strings zu ersetzen. Programmierer nutzen sie um flexiblen und lesbaren Code zu schreiben.

## So geht's:

In Bash können Sie auf diese einfache Weise Variablen innerhalb von Strings interpolieren:

```Bash
name="Welt"
echo "Hallo, $name"
```
Dieses Skript würde `"Hallo, Welt"` ausgeben.

Oder Sie können Parameter verwenden:

```Bash
read -p "Bitte geben Sie Ihren Namen ein: " name
echo "Hallo, $name"
```

## Vertiefung:

Die Interpolation von Strings ist nicht nur auf Bash beschränkt, sondern findet sich in vielen anderen Programmiersprachen wie Perl und Ruby. Alternativ kann in Bash die `printf` Funktion verwendet werden, um eine Formatierung zu ermöglichen. Zum Beispiel, `printf "Hallo, %s\n" $name`.

Es ist wichtig sich zu merken, dass eine Variable nicht interpoliert wird, wenn sie in einfachen Anführungszeichen steht:

```Bash
echo 'Hallo, $name'
```

Dies gibt genau so aus, wie es aussieht: `Hallo, $name`.

## Siehe auch:

Für weitere Information zur String-Interpolation in Bash, siehe:

- [Bash-Handbuch bei GNU](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Advanced Bash-Scripting Guide](http://tldp.org/LDP/abs/html/string-manipulation.html)