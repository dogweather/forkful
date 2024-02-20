---
date: 2024-01-20 17:55:58.940638-07:00
description: "Das Lesen von Befehlszeilenargumenten erlaubt es Skripten, verschiedene\
  \ Eingaben zu verarbeiten und anzupassen. Programmierer nutzen dies, um flexible\
  \ und\u2026"
lastmod: 2024-02-19 22:05:13.263880
model: gpt-4-1106-preview
summary: "Das Lesen von Befehlszeilenargumenten erlaubt es Skripten, verschiedene\
  \ Eingaben zu verarbeiten und anzupassen. Programmierer nutzen dies, um flexible\
  \ und\u2026"
title: Lesen von Kommandozeilenargumenten
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen von Befehlszeilenargumenten erlaubt es Skripten, verschiedene Eingaben zu verarbeiten und anzupassen. Programmierer nutzen dies, um flexible und interaktive Tools zu erstellen.

## Wie geht das:
```Fish Shell
# Einfacher Zugriff auf Argumente
for arg in $argv
    echo "Argument: $arg"
end

# Ausgabe, falls Sie das Skript mit „fish myscript.fish one two three“ ausführen:
Argument: one
Argument: two
Argument: three

# Überprüfen der Anzahl der Argumente
if count $argv > 0
    echo "Das erste Argument ist: $argv[1]"
else
    echo "Keine Argumente angegeben."
end
```

## Tiefer Eintauchen:
In Fish Shell ist `$argv` ein Array, das die Befehlszeilenargumente enthält. Historisch gesehen war der Zugriff auf Befehlszeilenargumente immer ein wichtiger Bestandteil der Unix-Philosophie, um kleine, aber mächtige Programme zu bilden, die miteinander kombiniert werden können. Andere Shells wie Bash nutzen `$1`, `$2` etc., um auf Argumente zuzugreifen. Fish sorgt hier für mehr Lesbarkeit, indem `$argv` wie ein Array behandelt wird. Die Indexierung beginnt bei 1 statt bei 0, was für Anfänger intuitiver sein kann. 

Details wie die Verwendung von Flags oder spezielle Optionsparser sind ebenfalls möglich, erfordern jedoch zusätzliche Befehle oder Extras wie `argparse`.

## Weiterführende Links:
- Fish Shell Dokumentation: https://fishshell.com/docs/current/index.html
- Fish Shell Tutorial für Argument Parsing: https://fishshell.com/docs/current/tutorial.html#tut_scripting
- Argparse Dokumentation: https://fishshell.com/docs/current/cmds/argparse.html
