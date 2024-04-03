---
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:01:48.206450-07:00
description: "Code in Funktionen zu organisieren bedeutet, Teile eines Skripts zu\
  \ b\xFCndeln, um spezifische Aufgaben zu erledigen. Wir tun dies, da es den Code\
  \ leichter\u2026"
lastmod: '2024-03-13T22:44:54.314990-06:00'
model: gpt-4-0125-preview
summary: "Code in Funktionen zu organisieren bedeutet, Teile eines Skripts zu b\xFC\
  ndeln, um spezifische Aufgaben zu erledigen."
title: Organisation von Code in Funktionen
weight: 18
---

## Was & Warum?
Code in Funktionen zu organisieren bedeutet, Teile eines Skripts zu bündeln, um spezifische Aufgaben zu erledigen. Wir tun dies, da es den Code leichter lesbar, testbar und wiederverwendbar macht – niemand möchte durch einen Sumpf von Codespaghetti waten.

## Wie geht das:
In Fish schreibst du eine Funktion mit dem Schlüsselwort `function`, gibst ihr einen Namen und beendest sie mit `end`. Hier ist eine einfache:

```fish
function hello
    echo "Hallo, Welt!"
end

hello
```

Ausgabe:
```
Hallo, Welt!
```

Jetzt lassen wir sie einen Benutzer begrüßen:

```fish
function greet
    set user (whoami)
    echo "Hey da, $user!"
end

greet
```

Ausgabe:
```
Hey da, dein_benutzername!
```

Um sie über Sitzungen hinweg zu speichern, verwende `funcsave greet`.

## Tiefer Eintauchen
Fish Shell Funktionen sind wie Mini-Skripte – du kannst so ziemlich alles hineinpacken. Historisch gesehen hat das Konzept von Funktionen in Shell-Skripten unzählige Stunden wiederholender Eingaben und Debugging gespart. Anders als Programmiersprachen wie Python, sind Shell-Funktionen mehr auf Bequemlichkeit als auf Struktur ausgerichtet.

Manche Shells, wie Bash, verwenden `function` oder einfach direkte Klammern. Fish hält sich an `function ... end`— klar und lesbar. Innerhalb von Fish-Funktionen hast du alle Funktionen: Parameter, lokale Variablen mit `set -l`, und du kannst sogar eine Funktion innerhalb einer anderen Funktion definieren.

Du benötigst keinen `return` Wert, da Fish darauf nicht großen Wert legt; die Ausgabe deiner Funktion ist ihre Rückgabe. Und wenn du persistente Funktionen für zukünftige Sitzungen haben möchtest, erinnere dich an `funcsave`.

## Siehe auch

- Das Fish-Tutorial zu Funktionen: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### Funktionsbefehle

- [function](https://fishshell.com/docs/current/cmds/function.html) — Eine Funktion erstellen
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — Funktionen anzeigen oder löschen
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Die Definition einer Funktion im Autoload-Verzeichnis des Benutzers speichern
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Eine Funktion interaktiv bearbeiten
