---
title:                "Code in Funktionen organisieren"
date:                  2024-01-26T01:10:10.229509-07:00
model:                 gpt-4-1106-preview
simple_title:         "Code in Funktionen organisieren"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Organisieren von Code in Funktionen bedeutet, Teile eines Skripts zusammenzufassen, um spezifische Aufgaben auszuführen. Wir machen dies, da es den Code leichter lesbar, testbar und wiederverwendbar macht — niemand möchte sich durch einen Sumpf aus Codespaghetti wühlen.

## Wie geht das:
In Fish schreibt man eine Funktion mit dem Schlüsselwort `function`, gibt ihr einen Namen und beendet sie mit `end`. Hier ist eine einfache:

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

Nun lassen Sie uns eine Begrüßung für einen Benutzer machen:

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

Um es über Sitzungen hinweg zu speichern, verwenden Sie `funcsave greet`.

## Tiefergehend
Fish-Shell-Funktionen sind wie Mini-Skripte – man kann so ziemlich alles hineinstecken. Historisch gesehen hat das Konzept der Funktionen in Skriptsprachen unzählige Stunden wiederholender Eingaben und Fehlersuche gespart. Im Gegensatz zu Programmiersprachen wie Python geht es bei Shell-Funktionen mehr um Bequemlichkeit als um Struktur.

Einige Shells, wie Bash, verwenden `function` oder einfach gerade Klammern. Fish bleibt bei `function ... end` – klar und lesbar. Innerhalb von Fish-Funktionen haben Sie alle Extras: Parameter, lokale Variablen mit `set -l`, und Sie können sogar eine Funktion innerhalb einer anderen Funktion definieren.

Sie benötigen keinen `return`-Wert, weil Fish darauf nicht großen Wert legt; die Ausgabe Ihrer Funktion ist ihr Rückgabewert. Und wenn Sie dauerhafte Funktionen für zukünftige Sitzungen verfügbar machen möchten, denken Sie an `funcsave`.

## Siehe auch
- Das Fish-Tutorial zu Funktionen: https://fishshell.com/docs/current/tutorial.html#tut_functions
- Die Fish-Dokumentation für `function`: https://fishshell.com/docs/current/cmds/function.html
- Ein umfangreicher Leitfaden zum Schreiben von Funktionen in Fish: https://fishshell.com/docs/current/index.html#syntax-function
