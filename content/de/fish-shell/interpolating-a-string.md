---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Zeichenketten-Interpolation ist eine Technik, bei der wir Werte innerhalb von Zeichenketten einfügen. Warum? Es erleichtert das Kodieren und macht Code deutlicher und verständlicher.

## Wie geht das? 

Im Fish Shell kannst du den Namen einer Variablen direkt in deinem String platzieren, um Interpolation zu erreichen. Hier ist ein einfaches Beispiel:

```Fish Shell
set name "Freddy"
echo "Hallo $name"
```

Der Ausgabe wäre dann:

```Fish Shell
Hallo Freddy
```

Der Wert der Variablen `name` wurde in die Zeichenkette eingefügt.

## Tiefere Einblicke

Die Zeichenketteninterpolation hat eine lange Programmierhistorie und existiert in viele verschiedene Sprachen. Fish Shell macht es besonders einfach: keine Notwendigkeit für Konkatenation oder spezielle Formatierungszeichenketten!

Alternativen in Fish Shell? Du kannst mehrere Argumente an `echo` übergeben und sie werden zusammen mit Leerzeichen verbunden.

```Fish Shell
set nachname "Krüger"
echo "Hallo" $name $nachname
```

Der Ausgabe:

```Fish Shell
Hallo Freddy Krüger
```

Tiefgreifende implementierungsdetails? Nun, es ist verankert in den Grundlagen der Fish Shell und von vielen anderen Shells übernommen.

## Siehe Auch

- Die Fish Shell Dokumentation für [Zeichenketteninterpolation](https://fishshell.com/docs/current/index.html#expand)
- [Tutorial zu Fish Shell Variablen](https://fishshell.com/docs/current/tutorial.html#tut_variables)

Nutze sie und verschönere deinen Fish Code mit Zeichenketteninterpolation. Alles ist Fisch!