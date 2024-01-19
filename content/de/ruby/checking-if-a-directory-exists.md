---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Lua: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Prüfen, ob ein Verzeichnis existiert, sehen wir nach, ob ein bestimmtes Verzeichnis in unserem Dateisystem tatsächlich vorhanden ist. Programmierer tun dies, um potenzielle Fehler in ihrem Code zu vermeiden, z. B. beim Versuch, auf ein nicht existierendes Verzeichnis zuzugreifen.

## So geht's:

Prüfen Sie, ob ein Verzeichnis in Ruby existiert, ist ziemlich einfach. Nutzen Sie die Methode `Dir.exist?`. Hier ist ein einfacher Codeausschnitt:

```Ruby
if Dir.exist?('testdirectory')
  puts "Das Verzeichnis existiert."
else
  puts "Das Verzeichnis existiert nicht."
end
```

Das gibt entweder "Das Verzeichnis existiert." aus, wenn das Verzeichnis vorhanden ist, oder "Das Verzeichnis existiert nicht." wenn es nicht ist.

## Tief tauchen:

Historisch gesehen, `Dir.exist?` ist in Ruby seit Version 1.9 verfügbar. Davor verwendeten Entwickler die Methode `File.directory?`, die immer noch nutzbar und funktional äquivalent ist:

```Ruby
if File.directory?('testdirectory')
  puts "Das Verzeichnis existiert."
else
  puts "Das Verzeichnis existiert nicht."
end
```

Eine weitere Alternative ist die Methode `File.exist?`, die jedoch auch Dateien berücksichtigt, nicht nur Verzeichnisse:

```Ruby
if File.exist?('testdirectory')
  puts "Datei oder Verzeichnis existiert."
else
  puts "Datei oder Verzeichnis existiert nicht."
end
```

Hinsichtlich der Implementierungsdetails wird `Dir.exist?` intern das Systemaufruf `stat` verwenden, um Informationen über das Verzeichnis abzurufen.

## Siehe auch:

- [Die offizielle Dokumentation zu `Dir.exist?`](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F)
- [Die offizielle Dokumentation zu `File.directory?`](https://ruby-doc.org/core-2.7.1/File.html#method-c-directory-3F)
- [Die offizielle Dokumentation zu `File.exist?`](https://ruby-doc.org/core-2.7.1/File.html#method-c-exist-3F)
- [Eine detaillierte Diskussion zur Verwendung von `Dir.exist?` vs `File.directory?`](https://stackoverflow.com/questions/1755665/determine-whether-a-directory-exists-with-ruby) in StackOverflow.