---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:10.074658-07:00
description: "Das \xDCberpr\xFCfen, ob ein Verzeichnis in Fish Shell existiert, erm\xF6\
  glicht es Skripten, Entscheidungen basierend auf der An- oder Abwesenheit von\u2026"
lastmod: '2024-03-13T22:44:54.323382-06:00'
model: gpt-4-0125-preview
summary: "Das \xDCberpr\xFCfen, ob ein Verzeichnis in Fish Shell existiert, erm\xF6\
  glicht es Skripten, Entscheidungen basierend auf der An- oder Abwesenheit von Verzeichnisstrukturen\
  \ zu treffen."
title: "\xDCberpr\xFCfung, ob ein Verzeichnis existiert"
weight: 20
---

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
