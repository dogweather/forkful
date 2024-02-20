---
date: 2024-01-20 17:34:47.349514-07:00
description: "Beim Verketten von Strings geht es darum, sie aneinanderzureihen, um\
  \ einen l\xE4ngeren String zu bilden. Programmierer machen das, um dynamische Texte\
  \ zu\u2026"
lastmod: 2024-02-19 22:05:13.238151
model: gpt-4-1106-preview
summary: "Beim Verketten von Strings geht es darum, sie aneinanderzureihen, um einen\
  \ l\xE4ngeren String zu bilden. Programmierer machen das, um dynamische Texte zu\u2026"
title: "Zeichenketten verkn\xFCpfen"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Verketten von Strings geht es darum, sie aneinanderzureihen, um einen längeren String zu bilden. Programmierer machen das, um dynamische Texte zu erzeugen, Eingaben zu formatieren oder einfach Informationen zusammenzuführen.

## So geht’s:

Im Fish Shell fügt man Strings mit einfachem Nebeneinanderstellen zusammen:

```Fish Shell
set str1 "Hallo, "
set str2 "wie geht's?"
echo $str1$str2
```

Ausgabe: `Hallo, wie geht's?`

Variablen und Literale kann man genauso leicht kombinieren:

```Fish Shell
set name "Anja"
echo "Guten Tag, "$name"!"
```

Ausgabe: `Guten Tag, Anja!`

Auch das Anhängen mit der `string` Funktion ist möglich:

```Fish Shell
set greeting "Herzlich Willkommen, "
set user "Tobias"
string join '' $greeting$user
```

Ausgabe: `Herzlich Willkommen, Tobias`

## Deep Dive

Historisch gesehen ist das Verketten von Strings eine Standardfunktion in den meisten Programmiersprachen und Shell-Skripts. Im Gegensatz zu manchen anderen Shells, die Operatoren wie `+` oder `.`, oder Funktionen wie `concat()` benötigen, macht Fish es sehr intuitiv – durch einfaches Aneinanderreihen der Strings.

Alternativen im Fish Shell sind die direkte Verwendung der `echo`- oder `printf`-Befehle zum Zusammenführen der Strings:

```Fish Shell
echo "Hallo" "Welt" # Standardausgabe ist bereits verknüpft
printf "%s%s\n" "Hallo" "Welt" # printf für formatierte Ausgabe
```

Hinsichtlich der Implementierung, durch den Wegfall von Konkatenierungsoperatoren, vermeidet Fish potenzielle Fehlerquellen und erhöht die Lesbarkeit des Codes.

## Siehe auch

- Die offizielle Fish-Shell-Dokumentation: [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- Fish Shell Tutorial für Anfänger: [Fish Shell für Anfänger](https://fishshell.com/docs/current/tutorial.html)
- Ein umfassender Leitfaden zu `string` Befehlen in Fish: [Fish Shell String Commands](https://fishshell.com/docs/current/cmds/string.html)
