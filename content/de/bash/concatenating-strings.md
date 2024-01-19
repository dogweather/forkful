---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

# **Bash und das Zusammenfügen von Zeichenketten: Schnell und Einfach**

## Was & Warum?
Das Zusammenfügen von Zeichenketten ("Concatenation") bedeutet, zwei oder mehr Strings zu einem einzelnen zusammenzumergen. Es wird in der Regel verwendet, um komplexe Nachrichten, Befehle, oder Anfragen zu generieren.

## Wie es geht:

Sie können ganz schnell Zeichenketten in Bash zusammenfügen. Mustercode und Ausgabe sind unten zu sehen.

```Bash
# Variablen deklarieren
str1="Hallo"
str2="Welt"

# Zeichenketten zusammenfügen
gruss="${str1}, ${str2}!"

# Ausgaben
echo $gruss
```

Ausgabe: `Hallo, Welt!`

## Tiefgehende Infos
Zeichenketten-Zusammenfügen hat eine lange Geschichte in der Programmierung und wird fast in jeder Sprache unterstützt, obwohl die Syntax variiert. In Bash, können Sie auch den `+=` Operator verwenden, um eine Zeichenkette an eine existierende Zeichenkette anzuhängen.

```Bash
str="Hallo"
str+=" Welt!"
echo $str
```

Ausgabe: `Hallo Welt!`

Einige Alternativen zum Zusammenfügen von Zeichenketten in Bash sind die Verwendung von `printf` Funktion oder den `paste` Befehl.

## Siehe auch
Falls Sie sich weiter informieren möchten, finden Sie [hier](https://wiki.bash-hackers.org/syntax/pe) eine gründliche Erklärung zur Parametererweiterung in Bash, welche das Zusammenfügen von Zeichenketten umfasst. Auch das GNU Bash Manual [hier](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion) könnte nützlich sein.