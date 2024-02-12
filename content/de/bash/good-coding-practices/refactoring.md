---
title:                "Refactoring"
aliases:
- /de/bash/refactoring.md
date:                  2024-01-26T01:16:31.534745-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/refactoring.md"
---

{{< edit_this_page >}}

## Was & Warum?
Refactoring ist der Prozess der Umstrukturierung bestehenden Computer-Codes, ohne dessen externes Verhalten zu verändern. Es ist eine wesentliche Praxis, um Komplexität zu reduzieren, die Wartbarkeit zu verbessern und Ihre Codebasis gesund und leichter verständlich zu halten, sowohl für aktuelle als auch für zukünftige Entwickler.

## Wie:
Betrachten wir ein einfaches Bash-Skript, das ein Refactoring benötigt. Es ist unhandlich, mit wiederholtem Code und schwer zu folgen:

```Bash
#!/bin/bash
echo "Geben Sie einen Dateinamen ein:"
read filename
if [ -f "$filename" ]; then
    echo "Datei existiert."
    count=$(grep -c "foo" "$filename")
    echo "Das Wort foo erscheint $count Mal."
else
    echo "Datei existiert nicht."
fi
```

Refactoring für Klarheit und Wiederverwendbarkeit könnte die Einführung von Funktionen und eine anmutigere Fehlerbehandlung beinhalten:

```Bash
#!/bin/bash

function file_exists() {
    [[ -f "$1" ]]
}

function count_occurrences() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "Geben Sie einen Dateinamen ein:"
    read -r filename
    echo "Geben Sie das zu suchende Wort ein:"
    read -r word

    if file_exists "$filename"; then
        count=$(count_occurrences "$word" "$filename")
        echo "Das Wort $word erscheint $count Mal."
    else
        echo "Datei existiert nicht." >&2
        exit 1
    fi
}

main "$@"
```

Die überarbeitete Version verwendet Funktionen, um die Lesbarkeit zu verbessern und eine potenzielle Wiederverwendung zu ermöglichen.

## Tiefere Einblicke:
Refactoring ist kein Konzept, das mit Bash oder sogar Hochsprachen begonnen hat; es ist so alt wie die Programmierung selbst. Der Begriff wurde im Buch "Refactoring: Improving the Design of Existing Code" von Martin Fowler im Jahr 1999 formalisiert, welches sich hauptsächlich auf objektorientierte Sprachen konzentrierte.

Im Kontext von Bash-Skripten bedeutet Refactoring oft, lange Skripte in Funktionen aufzuteilen, Wiederholungen durch Schleifen oder Bedingungen zu reduzieren und gängige Fallstricke zu vermeiden, wie das Nichtbeachten von Leerzeichen in Dateinamen. Alternativen zu Bash für Skripte, die zu komplex geworden sind, umfassen Python oder Perl, die bessere Datenstrukturen und Fehlerbehandlung für komplexe Aufgaben anbieten.

Bash-spezifisches Refactoring dreht sich mehr um die Einhaltung von Best Practices, wie das Zitieren von Variablen, die Verwendung von `[[ ]]` für Tests anstelle von `[ ]`, und die Bevorzugung von `printf` gegenüber `echo` für robuste Ausgaben. Implementierungsdetails drehen sich oft um die Einhaltung der Stilrichtlinien und die Verwendung von Tools wie `shellcheck` für die statische Analyse, um gängige Fehler zu erkennen.

## Siehe auch:
- [Google's Shell Style Guide](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, ein statisches Analysetool für Shell-Skripte](https://www.shellcheck.net/)
- [The Art of Command Line](https://github.com/jlevy/the-art-of-command-line)
