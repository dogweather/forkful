---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

HTML-Parsing ist das Lesen und Verstehen einer HTML-Datei durch Programme. Wir machen das, um Informationen wie Text, Links und Bilder aus Webseiten zu extrahieren.

## Wie es geht:

Hier ist ein einfacher HTML-Parser mit Fish Shell, unter Verwendung von `grep`:

```Fish Shell
function parse_html
    set file $argv[1]
    grep -oP '(?<=<title>).*(?=</title>)' $file
end
```

In diesem Beispiel extrahieren wir den Inhalt aus dem HTML-`<title>` Tag. Angenommen, wir haben eine HTML-Datei `example.html` mit `<title>Beispiel</title>`, so wird diese Code-Ausgabe so aussehen:

```Fish Shell
> parse_html example.html
Beispiel
```

## Vertiefung:

Beim HTML-Parsing gab es historisch viele Ansätze. Frühe Shell-Skripte haben oft Reguläre Ausdrücke (Regex) verwendet, aber das hat seine Einschränkungen. Komplexere HTML-Dokumente lassen sich mit Regex nicht einfach abbilden.

Als Alternative gibt es dedizierte HTML-Parser-Bibliotheken in fast jeder Programmiersprache. Sie sind genau gemacht für diese Aufgabe. Aber manchmal will man einfach nur schnell etwas extrahieren, und da wird `grep` in der Shell genutzt.

In unseren Fish Shell Beispiel nutzen wir das Perl-kompatible Regex von `grep` (`-P` Flag), um zwischen den `<title>` Tags zu suchen. Diese Lösung ist minimal, aber möglicherweise flüchtig bei komplexen HTML-Dokumenten.

## Siehe auch:

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [HTML Parsing in Python](https://docs.python.org/3/library/html.parser.html)
- [HTML Parsing in JavaScript](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)