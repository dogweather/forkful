---
aliases:
- /de/ruby/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:13.542914-07:00
description: "Das \xDCberpr\xFCfen, ob ein Verzeichnis in Ruby existiert, erm\xF6\
  glicht es Programmierern, die Pr\xE4senz eines Verzeichnisses zu verifizieren, bevor\
  \ sie\u2026"
lastmod: 2024-02-18 23:09:05.438545
model: gpt-4-0125-preview
summary: "Das \xDCberpr\xFCfen, ob ein Verzeichnis in Ruby existiert, erm\xF6glicht\
  \ es Programmierern, die Pr\xE4senz eines Verzeichnisses zu verifizieren, bevor\
  \ sie\u2026"
title: "\xDCberpr\xFCfung, ob ein Verzeichnis existiert"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis in Ruby existiert, ermöglicht es Programmierern, die Präsenz eines Verzeichnisses zu verifizieren, bevor sie Operationen wie das Lesen von Dateien oder das Erstellen neuer Verzeichnisse durchführen. Dies ist entscheidend, um Fehler bei der Dateibehandlung zu vermeiden und die Zuverlässigkeit von Dateisystem-Manipulationen zu gewährleisten.

## Wie:
Die Standardbibliothek von Ruby bietet einfache Methoden, um die Existenz eines Verzeichnisses zu überprüfen. So machen Sie das mit reinem Ruby, ohne dass Sie Drittbibliotheken benötigen:

```ruby
require 'fileutils'

# Überprüfen, ob ein Verzeichnis existiert
if Dir.exist?('/pfad/zum/verzeichnis')
  puts 'Verzeichnis existiert.'
else
  puts 'Verzeichnis existiert nicht.'
end
```
Beispiel-Ausgabe:
```
Verzeichnis existiert.
```
Oder:
```
Verzeichnis existiert nicht.
```

Zusätzlich zur Verwendung von `Dir.exist?` können Sie auch die Methode `File.directory?` nutzen, die `true` zurückgibt, wenn der angegebene Pfad ein Verzeichnis ist:

```ruby
if File.directory?('/pfad/zum/verzeichnis')
  puts 'Verzeichnis existiert.'
else
  puts 'Verzeichnis existiert nicht.'
end
```
Sowohl `Dir.exist?` als auch `File.directory?` sind Teil der Standardbibliothek von Ruby und erfordern keine externen Gems, was sie zu bequemen und effizienten Optionen für die Überprüfung von Verzeichnissen macht.
