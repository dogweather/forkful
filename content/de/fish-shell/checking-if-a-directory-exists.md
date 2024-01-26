---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:56:11.766609-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Existiert das Verzeichnis oder nicht? Das ist keine philosophische Frage, sondern eine praktische Abfrage, die wir als Entwickler ständig machen, um Fehler zu vermeiden oder um zu entscheiden, ob wir ein neues Verzeichnis erstellen sollen.

## How to:
Prüfen, ob ein Verzeichnis existiert, mit dem `test`-Befehl. Einfach und schnell.

```Fish Shell
if test -d /pfad/zum/verzeichnis
    echo "Verzeichnis existiert."
else
    echo "Verzeichnis existiert nicht."
end
```

Beispielausgabe, wenn das Verzeichnis existiert:

```
Verzeichnis existiert.
```

Beispielausgabe, wenn das Verzeichnis nicht existiert:

```
Verzeichnis existiert nicht.
```

## Deep Dive
In den Anfangszeiten der Shell-Programmierung hatte jeder sein eigenes Set an Tricks, um solche Abfragen zu machen. Heute gibt uns `test -d` eine portable, klare Syntax für diesen Zweck. Alternativ könnte man auch `[[ -d /pfad/zum/verzeichnis ]] && echo "Existiert"` in anderen Shells verwenden, aber in Fish ist `test -d` vorzuziehen. Beachte, dass `-d` ausschließlich für Verzeichnisse ist und die Abfrage scheitert, wenn es sich um eine Datei handelt, selbst mit dem gleichen Namen.

## See Also:
- Fish Dokumentation zu `test`: https://fishshell.com/docs/current/commands.html#test 
- Ein Forum für Fish-Nutzer: https://fishshell.com/community.html 
- Ein Tutorial zu Filesystem-Tests in Fish: https://fishshell.com/docs/current/tutorial.html#tut_file_tests
