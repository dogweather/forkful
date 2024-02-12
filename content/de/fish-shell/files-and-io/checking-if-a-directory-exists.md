---
title:                "Überprüfung, ob ein Verzeichnis existiert"
aliases:
- /de/fish-shell/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:10.074658-07:00
model:                 gpt-4-0125-preview
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis in Fish Shell existiert, ermöglicht es Skripten, Entscheidungen basierend auf der An- oder Abwesenheit von Verzeichnisstrukturen zu treffen. Dies ermöglicht Aufgaben wie bedingte Dateioperationen, Protokollierung oder das Einrichten von Umgebungen. Diese Technik ist entscheidend für das Schreiben von robusten Skripten, die auf vorhersehbare Weise mit dem Dateisystem interagieren.

## Wie geht das:
Fish Shell verwendet den `test` Befehl, um Dateitypen und -eigenschaften zu überprüfen, einschließlich, ob ein Ziel ein Verzeichnis ist. Hier ist ein grundlegendes Muster, um zu überprüfen, ob ein Verzeichnis existiert:

```fish
if test -d /pfad/zum/verzeichnis
    echo "Verzeichnis existiert"
else
    echo "Verzeichnis existiert nicht"
end
```
Beispielausgabe:
```
Verzeichnis existiert
```

Für effizientere Datei- und Verzeichnisoperationen könnte man sich an externe Werkzeuge wie `fd` wenden, obwohl es häufiger zum Finden von Dateien und Verzeichnissen als nur zur Überprüfung deren Existenz verwendet wird. Die Kombination damit in Fish Skripting kann jedoch praktische Ergebnisse liefern:

```fish
set dir "/pfad/zur/suche"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "Verzeichnis existiert"
else
    echo "Verzeichnis existiert nicht"
end
```

Dieses `fd`-Beispiel sucht das Verzeichnis in einer bestimmten Tiefe, und `grep` überprüft die Übereinstimmung, was es vielseitig für nuancierte Überprüfungen macht. Jedoch, für den direkten Zweck der Existenzüberprüfung, ist das Festhalten an Fishs eingebautem `test` sowohl effizient als auch unkompliziert.
