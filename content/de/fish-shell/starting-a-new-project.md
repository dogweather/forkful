---
title:                "Einen neuen Projekt starten"
date:                  2024-01-20T18:03:45.968857-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen neuen Projekt starten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
Ein neues Projekt zu starten bedeutet, eine frische Codebasis anzulegen, um eine Idee zum Leben zu erwecken. Programmierer tun dies, um Problemlösungen zu entwickeln, Innovationen voranzutreiben oder einfach ihre Skills zu verbessern.

## How to:
Ein neues Projekt in Fish beginnen? Kein Problem. Zuerst ein Verzeichnis erstellen und dann rein wechseln:

```Fish Shell
mkdir mein_neues_projekt
cd mein_neues_projekt
```

Jetzt initialisieren wir ein Git-Repository, falls Versionierung gewünscht:

```Fish Shell
git init
```

Umweltfreundlich? Setzen wir einige Umgebungsvariablen:

```Fish Shell
set -Ux EDITOR code
set -Ux PROJECT_PATH (pwd)
echo $PROJECT_PATH
```

Ausgabe:

```
/home/dein_name/mein_neues_projekt
```

Du brauchst eine isolierte Umgebung für Python-Abhängigkeiten?

```Fish Shell
python3 -m venv venv
source venv/bin/activate.fish
```

Jetzt bist du startklar und kannst loscoden!

## Deep Dive
Fish, kurz für "friendly interactive shell", ist eine Smart-Shell, die auf Usability fokussiert ist. Seit 2005 gibt's Fish, es vereinfacht komplexe Shell-Operationen. Alternative Shells sind z.B. Bash, Zsh und PowerShell.

Wichtigste Features sind Autosuggestions und Syntax-Highlighting. Das macht Fish einzigartig – und produktiv. Die Konfiguration ist einfach, Frickeleien nicht nötig. Keinesfalls zu vergessen, Fish hat eine eigene Syntax, also nicht direkt kompatibel mit Bash-Skripten.

Vergleich? Bash ist der alte Hase, enorm weit verbreitet und dokumentiert. Zsh ist wie Bash, aber mit Extras. Fish setzt auf User-Freundlichkeit und Einfachheit, manchmal mit dem Trade-off der Kompatibilität zu Bash-Skripten.

Flexibilität gewünscht? Funktionen in Fish sind leichtgewichtig und modular:

```Fish Shell
function greet
    echo "Hallo $argv!"
end
```

Aktiviere die Funktion:

```Fish Shell
greet Welt
```

Ausgabe:

```
Hallo Welt!
```

## See Also
- Die offizielle Fish-Website: [fishshell.com](https://fishshell.com)
- Fish- Dokumentation: [fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- GitHub-Repository für Fish: [github.com/fish-shell/fish-shell](https://github.com/fish-shell/fish-shell)
- Eine umfangreiche Liste mit Fish-Plugins: [github.com/jorgebucaran/awesome-fish](https://github.com/jorgebucaran/awesome-fish)
- Plugin-Manager für Fish: [github.com/jorgebucaran/fisher](https://github.com/jorgebucaran/fisher)