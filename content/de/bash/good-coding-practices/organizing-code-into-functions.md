---
date: 2024-01-26 01:09:06.424881-07:00
description: 'Wie geht das: Erstelle eine einfache Funktion in Bash.'
lastmod: '2024-03-13T22:44:54.065100-06:00'
model: gpt-4-1106-preview
summary: Erstelle eine einfache Funktion in Bash.
title: Code in Funktionen organisieren
weight: 18
---

## Wie geht das:
Erstelle eine einfache Funktion in Bash:

```Bash
gruesse() {
  echo "Hallo, $1!"
}
```

Verwende sie, indem du die Funktion mit einem Parameter aufrufst:

```Bash
gruesse "Welt"  # Ausgabe: Hallo, Welt!
```

Funktionen können Werte zurückgeben, indem sie `return` für numerische Statuscodes verwenden (nicht für die eigentliche Datenrückgabe):

```Bash
addiere() {
  return $(($1 + $2))
}

addiere 3 4
echo $?  # Ausgabe: 7
```

Beachte, dass `$?` den Rückgabewert des letzten Befehls einfängt, der das numerische Ergebnis von `addiere` ist.

## Vertiefung
In Bash sind Funktionen seit den frühen Versionen ein Mittel zur Kompartimentierung von Code. Historisch gesehen entspricht die Verwendung von Funktionen den Prinzipien der strukturierten Programmierung, die in den 1960er Jahren eingeführt wurden, um die Codequalität zu verbessern.

Alternativen zu Funktionen umfassen das Einbinden von Skriptdateien oder die Verwendung von Aliases, aber diese bieten nicht dasselbe Maß an Modularität und Wiederverwendbarkeit.

Ein bemerkenswertes Implementierungsdetail in Bash ist, dass Funktionen Bürger erster Klasse sind; sie haben kein spezifisches Deklarations-Schlüsselwort wie `function` in anderen Sprachen, obwohl `function` in Bash optional für die Lesbarkeit ist. Auch der Geltungsbereich von Funktionen ist interessant – Variablen sind standardmäßig global, es sei denn, sie werden als lokal deklariert, was zu unerwartetem Verhalten führen kann, wenn es nicht ordnungsgemäß verwaltet wird.

## Siehe auch
- Bash-Handbuch über Shell-Funktionen: https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- Fortgeschrittene Bash-Skripterstellung: https://tldp.org/LDP/abs/html/functions.html
- "Pro Bash Programming: Scripting the GNU/Linux Shell" für vertiefende Konzepte und Praktiken der Funktionsskripterstellung.
